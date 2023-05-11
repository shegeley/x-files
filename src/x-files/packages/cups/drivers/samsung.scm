(define-module (x-files packages cups drivers samsung)
  #:use-module (x-files utils base)

  #:use-module (guix licenses)
  #:use-module (gnu services cups)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages)
  #:use-module (guix packages)

  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (guix build copy-build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)

  #:use-module (gnu packages gnome)

  #:use-module (oop goops)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)

  #:export (install-plan
            samsung-eula
            samsung-drivers))

(define samsung-eula
  ((@@ (guix licenses) license) "Samsung EULA"
   "https://www.samsung.com/us/common/software_eula.html"
   "End-user license agreement for Samsung electronics software product"))

(define install-plan
  (match-lambda
    ('samsung
     '(("x86_64/pstosecps" "lib/cups/filter/pstosecps")
       ("x86_64/rastertospl" "lib/cups/filter/rastertospl")
       ("noarch/share/ppd" "share/ppd")))))

(define-public samsung-drivers
  (package
   (name "samsung-drivers")
   (version "1.00.29")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "https://printersetup.ext.hp.com/TS/Files/RDS_XML/web_install_agent/linux/ULD_v" version ".tar.gz"))
            (sha256
             (base32
              "05c88nkbfdahx4sg1h99q7hkyp331jgliazjbxa8hmk824fqysf9"))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan (quote ,(install-plan 'samsung)) ;; has to be quoted to treat it as 2-d list, not set of sexps (tries to apply them)
      #:validate-runpath? #f))
   (supported-systems '("x86_64-linux"))
   (home-page "https://samsung.com")
   (synopsis "Proprietary samsung drivers")
   (description "Proprietary samsung drivers")
   (license (list samsung-eula))))
