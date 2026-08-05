(define-module (x-files packages remark42)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)  #:prefix license:)
  #:use-module (guix build-system copy)
  #:export (remark42))

(define target->remark42-arch
  '(("x86_64-linux"  . "amd64")
    ("aarch64-linux" . "arm64")))

(define target->remark42-hash
  '(("x86_64-linux"  . "0ng9clrachw6zg43pyagcxpia6yvpnp5x0c2swjv54pccp7y86cd")
    ("aarch64-linux" . "1ych5f2h8bf9ywgvmi2n4851k4w5gfaw3dd19bg5lc8dwb12vc2w")))

(define-public remark42
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->remark42-arch target))
         (hash   (assoc-ref target->remark42-hash target))]
    (package
      (name "remark42")
      (version "1.16.4")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/umputun/remark42/releases/download/v"
               version "/remark42.linux-" arch ".tar.gz"))
         (sha256
          (base32 hash))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        `'((,(string-append "remark42.linux-" arch) "bin/remark42"))))
      (supported-systems (map car target->remark42-arch))
    (home-page "https://remark42.com")
    (synopsis "Privacy focused lightweight commenting engine")
    (description
     "Remark42 is a self-hosted, lightweight, and simple commenting system.
It stores data in a single bolt database, requires no external dependencies,
and supports multiple auth providers.")
    (license license:expat))))
