(define-module (x-files packages ntfy-el)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define ntfy-tests
  (local-file
   (search-path %load-path "x-files/packages/aux/ntfy-smoke-test.el")))

(define-public emacs-ntfy
  (package
    (name "emacs-ntfy")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shombando/ntfy")
             (commit "8aeeb5f06968231b3324d35c1f610a00717be567")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dvr4w5s4pa69khx7iqmxmaf7anlyvv98sszkk79y5f3iknw9lfv"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:test-command
      #~(list "emacs" "-Q" "--batch" "-L" "."
              "-l" #$ntfy-tests "-f" "ert-run-tests-batch-and-exit")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-default-title
            (lambda _
              ;; The URL transport still references the removed variable.
              (substitute* "ntfy.el"
                (("ntfy-header") "ntfy-title"))))
          (add-after 'install 'installed-smoke-test
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "emacs" "-Q" "--batch"
                        "-L" (dirname (car (find-files #$output "^ntfy\\.el$")))
                        "-l" #$ntfy-tests
                        "-f" "ert-run-tests-batch-and-exit")))))))
    (home-page "https://github.com/shombando/ntfy")
    (synopsis "Send ntfy.sh push notifications from Emacs")
    (description
     "This package is an interface to the @url{https://ntfy.sh,ntfy} service
(or a self-hosted instance) for sending push notifications from Emacs.  It
provides interactive commands to publish ad-hoc messages, messages with custom
headers and tags, and URLs, using Emacs' built-in URL library.")
    (license license:expat)))
