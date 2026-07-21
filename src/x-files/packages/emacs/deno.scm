(define-module (x-files packages emacs deno)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages)
  #:use-module ((x-files packages deno) #:select (deno))
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
    (arguments (list #:tests? #f))
    (propagated-inputs (list emacs-eglot emacs-project))
    (home-page "https://github.com/mgmarlow/deno-ts-mode")
    (synopsis "Major mode for Deno")
    (description "`deno-ts-mode' is derived from `typescript-ts-mode' so it depends on the same TypeScript tree-sitter parsers.  Install both the TSX and TypeScript parsers for full deno syntax support (see README for full details).

This package helps solve some of the problems that arise from Deno and TypeScript sharing the same file extension.  If a Deno configuration file is found at project root, `deno-ts-mode' takes precedence over `typescript-ts-mode'.  Both .ts and .tsx file extensions are supported.")
    (license license:gpl3+)))

(define-public emacs-deno-mode
  (package
    (name "emacs-deno-mode")
    (version "v0.2.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/shegeley/deno-mode")
            (commit version)))
      (sha256 (base32 "0h4hafk9pq73lpdrrziwjwgba9vdr4q2hmlx2k669a3i65fgn52d"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Bake the absolute `deno' store path into the mode/REPL so they
          ;; never rely on $PATH.  `emacs-substitute-variables' rewrites the
          ;; defcustom default in place.
          (add-after 'unpack 'patch-deno-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((deno (search-input-file inputs "/bin/deno")))
                (emacs-substitute-variables "deno-mode.el"
                  ("deno-bin" deno))
                (emacs-substitute-variables "deno-repl.el"
                  ("deno-repl-command" deno))))))))
    (inputs (list deno))
    (propagated-inputs (list emacs-eglot emacs-project))
    (home-page "https://github.com/shegeley/deno-mode")
    (synopsis "Major mode for Deno, with a bundled Deno REPL")
    (description "@code{deno-mode} is derived from @code{typescript-ts-mode} and
@code{js-ts-mode}, so it depends on the same TypeScript tree-sitter parsers.  It
bundles @code{deno-repl.el}, a @code{comint} front-end for @code{deno repl}.
@code{deno-bin} and @code{deno-repl-command} are patched to the absolute
@code{deno} store path at build time.")
    (license license:gpl3+)))
