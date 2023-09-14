(define-module (x-files packages guile)
  #:use-module (guix gexp)

  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)

  #:export (guile-srfi-64-ext))

(define-public srfi-64-ext
  (package
   (name "srfi-64-ext")
   (version "v0.0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/shegeley/srfi-64-ext")
           (commit version)))
     (sha256
      (base32 "1wzpv9bhbqdnc7jbfszifkg0hbphsd2nh7dsdd5ywz1r46fixqfh"))))
   (build-system guile-build-system)
   (inputs '())
   (arguments
    (list
     #:source-directory "src"
     #:compile-flags '(list
                       "--r6rs"
                       "-Wunbound-variable"
                       "-Warity-mismatch")
     #:phases
     #~(modify-phases
        %standard-phases
        (add-after 'set-paths 'add-output-to-guile-load-paths
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (guile-version (target-guile-effective-version))
                            (scm-path (string-append out
                                                     "/share/guile/site/"
                                                     guile-version))
                            (go-path (string-append out
                                                    "/lib/guile/"
                                                    guile-version
                                                    "/site-ccache")))
                       (setenv "GUILE_LOAD_PATH"
                               (string-append scm-path ":"
                                              (getenv "GUILE_LOAD_PATH")))
                       (setenv "GUILE_LOAD_COMPILED_PATH"
                               (string-append
                                go-path ":"
                                (getenv "GUILE_LOAD_COMPILED_PATH")))))))))
   (native-inputs
    (list
     guile-3.0-latest
     guix))
   (synopsis
    "A little testing framework build around (srfi srfi-64)")
   (description
    "Simple (srfi srfi-64) wrappers from Andrew's Tropin RDE project to make testing easier in any Guile Scheme project")
   (license license:gpl3+)
   (home-page "https://github.com/shegeley/srfi-64-ext")))
