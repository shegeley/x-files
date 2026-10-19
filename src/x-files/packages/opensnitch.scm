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
              (substitute* "ebpf_prog/Makefile"
                (("\\$\\(CC\\) \\$\\(CFLAGS\\) -c \\$<")
                 (string-append
                  "$(CC) $(CFLAGS) -S -o $*.ll $<\n"
                  "\tsed -e '/^target datalayout/d'"
                  " -e 's/^target triple = .*/target triple = \"bpf\"/'"
                  " $*.ll | llvm-as -o $@")))))
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
    (synopsis "eBPF modules for the OpenSnitch daemon")
    (description
     "Kernel probes OpenSnitch uses to attribute connections, process
executions and DNS replies to processes, replacing the expensive @file{/proc} scan
done on every new connection in the @code{proc} monitor method.")
    (license license:gpl3+)))

(define-public opensnitch-ebpf-modules
  (make-opensnitch-ebpf-modules))

(define* (make-opensnitch-daemon/ebpf #:optional
                                      (modules opensnitch-ebpf-modules))
  "Return @code{opensnitch-daemon} with the eBPF objects of @var{modules}
installed in its @file{etc/opensnitchd}.  Guix's daemon already rewrites
@file{/etc/opensnitchd} to that directory, one of the daemon's module search
paths, so no configuration key is needed."
  (package
    (inherit opensnitch-daemon)
    (name "opensnitch-daemon-ebpf")
    (arguments
     (substitute-keyword-arguments (package-arguments opensnitch-daemon)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install-default-config 'install-ebpf-modules
              (lambda _
                (for-each (lambda (o)
                            (install-file o (string-append #$output "/etc/opensnitchd")))
                          (find-files #$(file-append modules "/lib/opensnitchd/ebpf")
                                      "\\.o$"))))))))))

(define-public opensnitch-daemon/ebpf
  (make-opensnitch-daemon/ebpf))
