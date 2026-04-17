(define-module (x-files packages emacs 1s)
  #:use-module ((guix licenses)        #:prefix license:)
  #:use-module ((guix packages)        #:select (package origin base32))
  #:use-module ((guix git-download)    #:select (git-fetch git-reference git-file-name git-version))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system)))

(define-public emacs-1s
  (let [(commit "8fcc503f94a1b765f59521e6afa7e988ad767837")
        (version "0.0.1")]
    (package
      (name "emacs-1s")
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sirikid/emacs-1s")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "1w11llyw9rlmvwjrzr3filhfsa7m025ch9ynfnc228w5zbccc9s9"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (home-page "https://github.com/sirikid/emacs-1s")
      (synopsis "Emacs major mode for OneScript/BSL (1C:Enterprise)")
      (description
       "Major mode for editing OneScript and BSL (1C:Enterprise scripting
language) files in Emacs.  Provides syntax highlighting, SMIE-based
indentation, and LSP integration via @code{bsl-ls.el} which can connect
to bsl-language-server.")
      (license license:gpl3+))))
