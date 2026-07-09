(define-module (x-files packages stalwart)
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"  . "stalwart-x86_64-unknown-linux-gnu")
    ("aarch64-linux" . "stalwart-aarch64-unknown-linux-gnu")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "0kp7izwiczbi1i5hg5gs6im6cjicsqjaiwg96jh8vq747gbyxv6h")
    ("aarch64-linux" . "1qz00g07y65nni82cl5m69b2fpzz1da0vakjz5hyfhf5bd42l9pz")))

(define-public stalwart
  (let* [(target  (or (%current-target-system) (%current-system)))
         (bin     (assoc-ref target->bin-name target))
         (hash    (assoc-ref target->hash target))
         (version "0.16.12")
         (uri     (string-append
                   "https://github.com/stalwartlabs/stalwart/"
                   "releases/download/v" version "/"
                   bin ".tar.gz"))]
    (package
      (name "stalwart")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:strip-binaries?   #f
        #:patchelf-plan     #~'(("stalwart" ("gcc")))
        #:install-plan      #~'(("stalwart" "/bin/"))
        #:validate-runpath? #f
        ;; The upstream release tarball ships the binary without the
        ;; executable bit, so restore it after installation.
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'make-executable
              (lambda* (#:key outputs #:allow-other-keys)
                (chmod (string-append (assoc-ref outputs "out")
                                      "/bin/stalwart")
                       #o555))))))
      (inputs (list `(,gcc "lib")))
      (supported-systems targets)
      (home-page "https://stalw.art")
      (synopsis "All-in-one mail and collaboration server")
      (description "Stalwart is an all-in-one mail and collaboration server
written in Rust.  It bundles SMTP, IMAP4, POP3, JMAP, ManageSieve, LMTP, and
CalDAV/CardDAV/WebDAV support in a single binary, together with a built-in
spam and phishing filter, DKIM/SPF/DMARC/ARC/DANE/MTA-STS, full-text search,
Sieve scripting, and a web administration interface.  This package installs
the upstream prebuilt release binary.")
      (license license:agpl3))))
