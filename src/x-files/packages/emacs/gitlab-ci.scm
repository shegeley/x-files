(define-module (x-files packages emacs gitlab-ci)
  #:use-module ((guix packages)           #:select (package))
  #:use-module ((guix gexp)               #:select (local-file))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((rde lib file)            #:select (find-file-in-load-path))
  #:use-module ((x-files packages emacs lab) #:select (emacs-lab))

  #:export (emacs-gitlab-ci))

(define emacs-gitlab-ci
  (package
    (name "emacs-gitlab-ci")
    (version "0.1.0")
    ;; Bundled in the channel rather than fetched from a separate repo.
    (source (local-file
             (find-file-in-load-path "x-files/packages/aux/gitlab-ci/gitlab-ci.el")))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list emacs-lab))
    (home-page "https://grigory.tech")
    (synopsis "Per-host token wiring for lab.el (emacs-lab)")
    (description
     "Sets @code{lab-config} (see @code{emacs-lab}) from plain per-host
token files under @code{gitlab-ci-token-directory}, so
@code{lab-watch-pipeline-for-last-commit} and friends authenticate without
prompting.  A host with no readable token file falls back to
@code{auth-source}.")
    (license license:gpl3+)))
