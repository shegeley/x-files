(define-module (x-files packages emacs deno)
    #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages)
  #:use-module (guix utils))

(define-public emacs-deno-ts-mode
  (package
    (name "emacs-deno-ts-mode")
    (version "20230912.202")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/" "deno-ts-mode-" version ".tar"))
       (sha256
        (base32 "0njh7wgbbckp9j09mcz2libz8i6m8px2qc2cj8li0npxqi7dm3v9"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-eglot emacs-project))
    (home-page "https://github.com/mgmarlow/deno-ts-mode")
    (synopsis "Major mode for Deno")
    (description "`deno-ts-mode' is derived from `typescript-ts-mode' so it depends on the same TypeScript tree-sitter parsers.  Install both the TSX and TypeScript parsers for full deno syntax support (see README for full details).

This package helps solve some of the problems that arise from Deno and TypeScript sharing the same file extension.  If a Deno configuration file is found at project root, `deno-ts-mode' takes precedence over `typescript-ts-mode'.  Both .ts and .tsx file extensions are supported.")
    (license license:gpl3+)))
