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
  (let ((commit "8c33f35b490a12a8d4bc57954a946fe4d12abb81")
        (version "3.10.0"))
    (package
      (name "emacs-lab")
      (version (git-version version "0" commit))
      (source
       (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/isamert/lab.el")
              (commit commit)))
        (sha256 (base32 "1r3z1i3rkk75qhhjlcgagvww45z1sn06b4cx4ann539yvf4k6asd"))
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
