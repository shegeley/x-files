(define-module (x-files packages opensnitch)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix utils)                   #:select (substitute-keyword-arguments))
  #:use-module ((guix build-system gnu)        #:select (gnu-build-system))
  #:use-module ((guix licenses)                #:prefix license:)

  #:use-module ((gnu packages networking)      #:select (opensnitch-daemon))
  #:use-module ((gnu packages linux)           #:select (linux-libre))
  #:use-module ((gnu packages llvm)            #:select (clang llvm))
  #:use-module ((gnu packages bison)           #:select (bison))
  #:use-module ((gnu packages flex)            #:select (flex))
  #:use-module ((gnu packages algebra)         #:select (bc))
  #:use-module ((gnu packages perl)            #:select (perl))
  #:use-module ((gnu packages elf)             #:select (elfutils))
  #:use-module ((gnu packages tls)             #:select (openssl))
  #:use-module ((gnu packages compression)     #:select (zlib))

  #:export (make-opensnitch-ebpf-modules
            make-opensnitch-daemon/ebpf))

(define* (make-opensnitch-ebpf-modules #:optional (kernel linux-libre))
  "Return OpenSnitch's eBPF process-monitor objects built against
@var{kernel}.  They are not CO-RE: kprobe offsets come from @var{kernel}'s
configured headers, so the objects only load on that exact kernel."
  (package
    (name "opensnitch-ebpf-modules")
    (version (string-append (package-version opensnitch-daemon)
                            "-" (package-name kernel)
                            "-" (package-version kernel)))
    (source (package-source opensnitch-daemon))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'prepare-kernel-tree
            (lambda _
              (mkdir "../linux")
              (invoke "tar" "xf" #$(package-source kernel)
                      "-C" "../linux" "--strip-components=1")
              (for-each patch-shebang
                        (find-files "../linux"
                                    (lambda (file stat)
                                      (and (eq? 'regular (stat:type stat))
                                           (logtest #o100 (stat:perms stat))))))
              (copy-file #$(file-append kernel "/.config") "../linux/.config")
              (with-directory-excursion "../linux"
                (invoke "make" "olddefconfig")
                (invoke "make" "prepare"))))
          (add-before 'build 'retarget-bitcode-to-bpf
            ;; The Makefile compiles host (x86) IR and hands it to `llc
            ;; -march=bpf'; LLVM >= 17 rejects the mismatched DataLayout, so
            ;; drop it and let llc apply BPF's own.
            (lambda _
              (let ((emit-ir    "-S -o $*.ll $<")
                    (retarget   (string-join
                                 '("sed"
                                   "-e '/^target datalayout/d'"
                                   "-e 's/^target triple = .*/target triple = \"bpf\"/'"
                                   "$*.ll")))
                    (assemble   "llvm-as -o $@"))
                ;; %.bc rule: `$(CC) $(CFLAGS) -c $<'
                (substitute* "ebpf_prog/Makefile"
                  (("-c \\$<")
                   (string-append emit-ir "\n\t" retarget " | " assemble))))))
          (replace 'build
            (lambda _
              (let ((tree (canonicalize-path "../linux")))
                (with-directory-excursion "ebpf_prog"
                  (invoke "make"
                          (string-append "KERNEL_DIR=" tree)
                          (string-append "KERNEL_HEADERS=" tree)
                          ;; Linux >= 6.19 needs MS extensions for anonymous
                          ;; embedded structs; x86 ftrace.h wants fentry.
                          (string-append "EXTRA_FLAGS=-fms-extensions"
                                         " -Wno-microsoft-anon-tag"
                                         " -DCC_USING_FENTRY"))))))
          (replace 'install
            (lambda _
              (let ((dest (string-append #$output "/lib/opensnitchd/ebpf")))
                (mkdir-p dest)
                (for-each (lambda (o)
                            (invoke "llvm-strip" "-g" o)
                            (install-file o dest))
                          (find-files "ebpf_prog" "^opensnitch.*\\.o$"))))))))
    (native-inputs
     (list clang llvm bison flex bc perl elfutils openssl zlib))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://github.com/evilsocket/opensnitch")
    (synopsis "eBPF modules for @command{opensnitchd}")
    (description
     "Kernel probes OpenSnitch uses to attribute connections, process
executions and DNS replies to processes, replacing the expensive @file{/proc} scan
done on every new connection in the @code{proc} monitor method.")
    (license license:gpl3+)))

(define-public opensnitch-ebpf-modules
  (make-opensnitch-ebpf-modules))

(define* (make-opensnitch-daemon/ebpf #:optional
                                      (modules opensnitch-ebpf-modules)
                                      #:key
                                      (queue-connections? #t))
  "Return @code{opensnitch-daemon} with the eBPF objects of @var{modules}
installed in its @file{etc/opensnitchd}.  Guix's daemon already rewrites
@file{/etc/opensnitchd} to that directory, one of the daemon's module search
paths, so no configuration key is needed.

With @var{queue-connections?} @code{#f} the daemon no longer queues every new
outbound connection of the host to itself: it keeps tracking DNS replies, but
only judges the packets some other firewall rule sends to its netfilter queue
(e.g. a cgroup-scoped @code{queue num 0} without @code{bypass})."
  (package
    (inherit opensnitch-daemon)
    (name "opensnitch-daemon-ebpf")
    (arguments
     (substitute-keyword-arguments (package-arguments opensnitch-daemon)
       ((#:phases phases)
        #~(modify-phases #$phases
            #$@(if queue-connections?
                   '()
                   (list
                    #~(add-before 'build 'judge-only-externally-queued-packets
                        (lambda* (#:key import-path #:allow-other-keys)
                          (with-directory-excursion
                              (string-append "src/" import-path "/firewall/nftables")
                            (substitute* "nftables.go"
                              (("n\\.QueueConnections\\(common\\.EnableRule, common\\.EnableRule\\)")
                               "error(nil), error(nil)"))
                            ;; the rules checker expects 2 connection + 1 DNS rule
                            (substitute* "monitor.go"
                              (("nRules != 3") "nRules != 1")))))))
            (add-before 'build 'read-env-at-connection-time
              ;; The exec-event cache keeps the environment of a PID's first
              ;; exec, so process.env.* rules miss variables a wrapper set
              ;; before exec'ing the real binary; re-read it per connection,
              ;; as the proc monitor does.
              (lambda* (#:key import-path #:allow-other-keys)
                (with-directory-excursion
                    (string-append "src/" import-path "/procmon/ebpf")
                  (substitute* "find.go"
                    (("(\t\tproc = &ev\\.Proc\n)" all)
                     (string-append all "\t\tproc.ReadEnv()\n"))))))
            (add-after 'install-default-config 'install-ebpf-modules
              (lambda _
                (for-each (lambda (o)
                            (install-file o (string-append #$output "/etc/opensnitchd")))
                          (find-files #$(file-append modules "/lib/opensnitchd/ebpf")
                                      "\\.o$"))))))))))

(define-public opensnitch-daemon/ebpf
  (make-opensnitch-daemon/ebpf))
