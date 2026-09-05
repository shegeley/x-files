(define-module (x-files packages geckodriver)
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (first))
  #:use-module ((guix licenses) #:prefix license:)
  #:export (geckodriver-for
            geckodriver))

;; geckodriver -- WebDriver server for Firefox (Gecko counterpart to
;; chromedriver, see (g-files packages chromedriver)).
;;
;; Unlike Chrome/chromedriver, geckodriver has no tight per-build version
;; lock: Mozilla's own compat docs
;; (https://firefox-source-docs.mozilla.org/testing/geckodriver/Support.html)
;; say any reasonably current geckodriver works with Firefox 57 and later, and
;; explicitly recommend always using the LATEST geckodriver release rather
;; than pinning to a specific Firefox build.  So "moving hand-in-hand with
;; Firefox" here means something different than chromedriver-for's exact
;; build-matching: always track the newest geckodriver, and assert whatever
;; Firefox package it's meant to drive actually clears geckodriver's minimum
;; supported version -- erroring loudly rather than silently building a driver
;; against a Firefox too old for it to work at all.

(define %geckodriver-version "0.37.1")

;; Firefox 57 = the oldest version geckodriver supports at all (pre-57 lacks
;; the multi-process/e10s architecture geckodriver depends on). Checked
;; against Mozilla's own compat docs, cited above.
(define %geckodriver-minimum-firefox-version 57)

(define target->gecko-target
  '(("x86_64-linux"  . "linux64")
    ("aarch64-linux" . "linux-aarch64")))

(define targets (map car target->gecko-target))

(define target->hash
  '(("x86_64-linux"  . "0zlzw0hxxbs7ls1wwpv36lbrkkm3sd43p14imrif30srm47165g8")
    ("aarch64-linux" . "0331mlnzdwg9zpbdh5fnhvwv6vpnvx2b77skardsvyr22jahpncg")))

(define (firefox-major-version firefox)
  (string->number (first (string-split (package-version firefox) #\.))))

(define* (geckodriver-for #:optional firefox)
  "Build geckodriver, the WebDriver server for Firefox.  When FIREFOX is
given, assert its version clears @code{%geckodriver-minimum-firefox-version}
before building -- geckodriver has no per-build version lock the way
chromedriver does, so \"tracking\" Firefox here means always using the latest
geckodriver release while refusing to silently pair it with a Firefox too old
to work at all."
  (when firefox
    (let ((major (firefox-major-version firefox)))
      (unless (>= major %geckodriver-minimum-firefox-version)
        (error
         (string-append
          "geckodriver " %geckodriver-version " requires Firefox >= "
          (number->string %geckodriver-minimum-firefox-version)
          ", but " (package-name firefox) " " (package-version firefox)
          " was given.")))))
  (let* [(target       (or (%current-target-system) (%current-system)))
         (gecko-target (assoc-ref target->gecko-target target))
         (hash         (assoc-ref target->hash target))
         (version      %geckodriver-version)
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

(define-public geckodriver
  ;; Convenience default with no Firefox version check, for callers that
  ;; don't need to pair it with a specific Firefox package.
  (geckodriver-for))
