(define-module (x-files packages esquema)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix git-download) #:select (git-fetch
                                              git-reference
                                              git-file-name))
  #:use-module ((guix build-system gnu) #:select (gnu-build-system))
  #:use-module ((guix gexp) #:select (gexp))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages) #:select (search-patches))
  #:use-module ((gnu packages linux) #:select (libseccomp libcap))
  #:use-module ((gnu packages guile) #:select (guile-3.0))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:export (esquema))

;;; Esquema — a rootless, Guile-native container runtime (namespaces + seccomp
;;; allowlist + capability drop + cgroup limits, "containers as first-class
;;; Scheme objects").  Upstream: https://codeberg.org/berkeley/esquema.
;;;
;;; Build = a C shared library (libesquema.so, needs libseccomp/libcap) plus a
;;; Guile FFI layer under (esquema …).  The FFI locates the .so via
;;; $ESQUEMA_LIBDIR (set it to <esquema>/lib), so the g-files jail backend
;;; launches guile with ESQUEMA_LIBDIR + -L <esquema>/share/guile/site/3.0.

(define esquema
  (package
    (name "esquema")
    (version "0.0.0-1.9ed8d60")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/berkeley/esquema")
             (commit "9ed8d601526c1aa7710e93c839c32db6ec613608756a9dc78c06e60f13bd57fb")))
       (file-name (git-file-name name version))
       (sha256 (base32 "1d3fnd6fq9w8m0r8ckyryn99wc4a4li4jsmbahpy62cj7mvrvqp8"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; test-suite needs namespaces/rootfs
      #:make-flags #~(list "lib")       ; build only libesquema.so
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; plain Makefile, no ./configure
          (replace 'install
            (lambda _
              (let ((lib (string-append #$output "/lib"))
                    (scm (string-append #$output "/share/guile/site/3.0")))
                (mkdir-p lib)
                (install-file "libesquema.so" lib)
                (copy-recursively "scheme" scm)))))))
    (inputs (list libseccomp libcap))
    (native-inputs (list pkg-config guile-3.0))
    (synopsis "Rootless, Guile-native container runtime")
    (description
     "Esquema is a minimal, security-first, rootless container runtime written
in Scheme.  It confines a payload with user/mount/pid/net namespaces,
@code{pivot_root}, full capability drop with no-new-privs, a seccomp-BPF
allowlist and cgroup v2 limits, exposing containers as first-class Scheme
objects (@code{make-container} / @code{run-container}).")
    (home-page "https://codeberg.org/berkeley/esquema")
    (license license:gpl3+)))
