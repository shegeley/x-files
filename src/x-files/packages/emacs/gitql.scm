(define-module (x-files packages emacs gitql)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-gitql
  (package
    (name "emacs-gitql")
    (version "0.1.0")
    (source (local-file "../aux/gitql" #:recursive? #t))
    (build-system emacs-build-system)
    (arguments
     (list #:tests? #t
           #:test-command ''("emacs" "--batch"
                             "-l" "gitql.el"
                             "-l" "gitql-test.el"
                             "-f" "ert-run-tests-batch-and-exit")))
    (propagated-inputs (list emacs-transient))
    (home-page "https://github.com/AmrDeveloper/GQL")
    (synopsis "Interactive GQL (Git Query Language) buffer for Emacs")
    (description
     "Provides a comint-based interactive buffer for running GQL queries
against Git repositories, with transient menus for common operations.
GQL is a SQL-like language for querying .git files.")
    (license license:gpl3+)))
