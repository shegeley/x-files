(define-module (x-files packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-gptel
                                                   emacs-org-ql))
  #:use-module (gnu packages))

(define-public emacs-gptel-got
  ;; TODO: change url as merged https://codeberg.org/bajsicki/gptel-got/pulls/1/files
  (let [(c "9d45a77f9c593a295de58d798df5ae6cb1effb8c")
        (version "0.0.2")
        (hash "16jfk84vgs7dsgsac3iysm445nd7ldwfjsg5fgzd04i8x5ni4a8s")]
    (package
      (name "emacs-gptel-got")
      (version (git-version "0.0.2" "1" c))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/shegeley/gptel-got")
               (commit c)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ;; no tests
      (propagated-inputs (list emacs-gptel emacs-org-ql))
      (home-page "https://codeberg.org/bajsicki/gptel-got")
      (synopsis "LLM Tools for org-mode interaction")
      (description "This is a package which expands the functionality of gptel for interacting with org-mode")
      (license license:gpl3+))))
