(define-module (x-files packages remark42)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)  #:prefix license:)
  #:use-module (guix build-system copy)
  #:export (remark42))

(define-public remark42
  (package
    (name "remark42")
    (version "1.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/umputun/remark42/releases/download/v"
             version "/remark42.linux-amd64.tar.gz"))
       (sha256
        (base32 "1qwn7xhc7xw8bgkw3wsa8yd21ca5gfyy37fgkawh885c4gia01fz"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("remark42.linux-amd64" "bin/remark42"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://remark42.com")
    (synopsis "Privacy focused lightweight commenting engine")
    (description
     "Remark42 is a self-hosted, lightweight, and simple commenting system.
It stores data in a single bolt database, requires no external dependencies,
and supports multiple auth providers.")
    (license license:expat)))
