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
  '(("x86_64-linux"  . "16hdffz078v003icx6b7bmxkv8ay6gy5cadgpsxj60g362gs6zqg")
    ("aarch64-linux" . "11axjj0rl0l1kk3cnq1ndvqhjl6wy0kjn9szqcabmm5wsg92a7cf")))

(define-public remark42
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->remark42-arch target))
         (hash   (assoc-ref target->remark42-hash target))]
    (package
      (name "remark42")
      (version "1.18.0")
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
