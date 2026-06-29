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
  '(("x86_64-linux"  . "18y15f8abrixc3jskxp1i514854bqdrmcwmp38ygsl3wlc18kq6v")
    ("aarch64-linux" . "1zs7fnjg1az65v2s5r9mhzla5jpwc282pxm3fkm3rzybad44p938")))

(define-public stalwart
  (let* [(target  (or (%current-target-system) (%current-system)))
         (bin     (assoc-ref target->bin-name target))
         (hash    (assoc-ref target->hash target))
         (version "0.16.11")
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
