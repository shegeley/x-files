(define-module (x-files packages emacs canvas)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix git-download)
                #:select (git-fetch git-reference git-version git-file-name))
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module (guix gexp)
  #:use-module ((gnu packages emacs) #:select (emacs-next-pgtk))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-transient)))

(define-public emacs-canvas-keys
  (let ((commit "510c5fbc5b8d402a9b717ccd99f248c1b935eccf"))
    (package
      (name "emacs-canvas-keys")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Daskeladden/canvas-keys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1v5kj6xxnk7zklym8a3vns7c3a6ww6d5mk266kh6k89rlm5mpl7g"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:emacs emacs-next-pgtk
        #:test-command
        #~(list "emacs" "-Q" "--batch" "-L" "." "-L" "tests"
                "-l" "tests/canvas-keys-tests.el"
                "-l" "tests/canvas-keys-headers-tests.el"
                "-f" "ert-run-tests-batch-and-exit")))
      (propagated-inputs (list emacs-transient))
      (home-page "https://github.com/Daskeladden/canvas-keys")
      (synopsis "Shared key bindings and menus for Emacs canvas buffers")
      (description
       "Canvas Keys provides consistent key maps, Transient menus, and helpers
for packages that display Emacs 32 canvas images.")
      (license gpl3+))))
