(define-module (x-files packages emacs lab)
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-request
                                                    emacs-promise
                                                    emacs-async-await))
  #:use-module ((gnu packages emacs-build) #:select (emacs-s
                                                      emacs-f
                                                      emacs-compat))

  #:export (emacs-lab))

(define-public emacs-lab
  (let ((commit "180148f2b6fe6e52149decf70dadfd5ac51b3c0a")
        (version "3.10.0"))
    (package
      (name "emacs-lab")
      (version (git-version version "1" commit))
      (source
       (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/isamert/lab.el")
              (commit commit)))
        (sha256 (base32 "04zykg62nl67rw7kblmi64mxndh28bamivq2xnbxflyc3iw5zsq0"))
        (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs
       (list emacs-request
             emacs-s
             emacs-f
             emacs-compat
             emacs-promise
             emacs-async-await))
      (home-page "https://github.com/isamert/lab.el")
      (synopsis "GitLab integration for Emacs")
      (description
       "Interact with GitLab from Emacs: list and search projects, list and
create merge requests, do code review on a merge request diff, list and act
on CI/CD pipeline jobs, and watch a pipeline (@code{lab-watch-pipeline-for-last-commit})
for status-change notifications.")
      (license license:gpl3+))))
