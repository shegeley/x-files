(define-module (x-files packages geckodriver)
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:))

(define target->gecko-target
  '(("x86_64-linux"  . "linux64")
    ("aarch64-linux" . "linux-aarch64")))

(define targets (map car target->gecko-target))

(define target->hash
  '(("x86_64-linux"  . "0j75c8851n32b8fm20siysij7v7zm84i638n1i088rl1v4xy7m4h")
    ("aarch64-linux" . "0n21ml264dnpp74ab0jxbw9bwsl7mak7ip25ifknsxz1k2aw4sx1")))

(define-public geckodriver
  (let* [(target       (or (%current-target-system) (%current-system)))
         (gecko-target (assoc-ref target->gecko-target target))
         (hash         (assoc-ref target->hash target))
         (version      "0.37.0")
         (uri          (string-append
                        "https://github.com/mozilla"
                        "/geckodriver/releases/download/v" version
                        "/geckodriver-v" version "-" gecko-target
                        ".tar.gz"))]
    (package
      (name "gecko")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:validate-runpath? #f
        #:install-plan `'(("./geckodriver" "/bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _ (chmod "./geckodriver" #o755))))))
      (supported-systems targets)
      (home-page "https://geckodriver.org/")
      (synopsis "Selenium WebDriver for Firefox Automation")
      (description "geckodriver is a key component in the WebDriver framework, specifically designed to interact with Gecko-based browsers like Mozilla Firefox. It acts as a bridge between WebDriver clients and the Firefox browser, translating WebDriver commands into the Marionette protocol used by Firefox. This allows automated testing tools and scripts to control and interact with Firefox in a standardized way.This is vital for developers and testers who rely on consistent and reliable automation across different browsers. By implementing the latest WebDriver standards, geckodriver helps in maintaining up-to-date compatibility and features with Firefox")
      (license license:mpl2.0))))
