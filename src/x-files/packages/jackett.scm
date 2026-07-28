(define-module (x-files packages jackett)
  #:use-module ((gnu packages base)   #:select (glibc))
  #:use-module ((gnu packages bash)   #:select (bash-minimal))
  #:use-module ((gnu packages elf)    #:select (patchelf))
  #:use-module ((gnu packages gcc)    #:select (gcc))
  #:use-module ((gnu packages icu4c)  #:select (icu4c))
  #:use-module ((gnu packages tls)    #:select (openssl))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

;; Jackett ships self-contained .NET folder deployments (the whole runtime is
;; bundled next to the app: libcoreclr.so, libclrjit.so, libhostfxr.so, ...).
;; So there is nothing to build; we only need to make the bundled ELF loadable
;; on Guix: patch the interpreter of the native launchers and hand the bundled
;; .so their non-bundled dependencies (libstdc++/libgcc_s, plus the OpenSSL and
;; ICU that libSystem.*.Native.so dlopen at runtime) via a wrapper's
;; LD_LIBRARY_PATH.

(define target->tarball
  '(("x86_64-linux"  . "Jackett.Binaries.LinuxAMDx64.tar.gz")
    ("aarch64-linux" . "Jackett.Binaries.LinuxARM64.tar.gz")))

(define targets (map car target->tarball))

(define target->hash
  '(("x86_64-linux"  . "0gyfrz7zzqqpcza98xlppfyicfx3gi74pxqfwvc8jlqyzbqiszbl")
    ("aarch64-linux" . "0xxgllrglikxqabv2iizqy06f44a024l2jvvg7z2hgb55q665n2q")))

(define-public jackett
  (let* ((target  (or (%current-target-system) (%current-system)))
         (tarball (assoc-ref target->tarball target))
         (hash    (assoc-ref target->hash target))
         (version "0.24.2288"))
    (package
      (name "jackett")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/Jackett/Jackett/releases/download/"
                      "v" version "/" tarball))
                (file-name (string-append "jackett-" version ".tar.gz"))
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        ;; The wrapper injects the library path; the bundled .so keep no RUNPATH.
        #:validate-runpath? #f
        ;; Stripping the bundled runtime .so buys nothing and only risks
        ;; corrupting them.
        #:strip-binaries? #f
        #:phases
        #~(modify-phases %standard-phases
            ;; `unpack' chdirs into the single top-level `Jackett/'.
            (add-after 'unpack 'set-interpreter
              (lambda _
                (let ((ld (car (find-files (string-append #$glibc "/lib")
                                           "^ld-linux.*\\.so.*"))))
                  (for-each
                   (lambda (exe)
                     (chmod exe #o755)
                     (invoke "patchelf" "--set-interpreter" ld exe))
                   '("jackett" "JackettUpdater" "createdump")))))
            (replace 'install
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((share (string-append #$output "/share/jackett"))
                       (bin   (string-append #$output "/bin"))
                       (exe   (string-append bin "/jackett"))
                       (libs  (string-join
                               (list (string-append
                                      (assoc-ref inputs "glibc") "/lib")
                                     (string-append
                                      (assoc-ref inputs "gcc") "/lib")
                                     (string-append
                                      (assoc-ref inputs "openssl") "/lib")
                                     (string-append
                                      (assoc-ref inputs "icu4c") "/lib"))
                               ":")))
                  (mkdir-p share)
                  (copy-recursively "." share)
                  (mkdir-p bin)
                  (call-with-output-file exe
                    (lambda (port)
                      (format port "#!~a
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"
exec \"~a/jackett\" \"$@\"~%"
                              (string-append (assoc-ref inputs "bash-minimal")
                                             "/bin/bash")
                              libs share)))
                  (chmod exe #o555)))))))
      (native-inputs (list patchelf))
      (inputs (list bash-minimal
                    glibc
                    `(,gcc "lib")
                    icu4c
                    openssl))
      (supported-systems targets)
      (home-page "https://github.com/Jackett/Jackett")
      (synopsis "Proxy server translating tracker queries into Torznab/TorrentPotato")
      (description "Jackett works as a proxy server: it translates queries from
apps (Sonarr, Radarr, Lidarr, and other @acronym{PVR, Personal Video Recorder}
tools) into tracker-site-specific HTTP queries, parses the HTML or JSON
response, then sends results back to the requesting software.  It exposes a
Torznab and a TorrentPotato @acronym{API} and supports hundreds of public and
private trackers.  This package installs the upstream self-contained .NET
build.")
      (license license:gpl2))))

jackett
