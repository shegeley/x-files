(define-module (x-files packages emacs tmsu)
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((gnu packages file-systems) #:select (tmsu))

  #:export (emacs-tmsu))

;; emacs-tmsu -- dired/org-mode integration for the `tmsu' file-tagging CLI
;; (see (gnu packages file-systems) tmsu, packaged upstream in mainline
;; Guix already).  The `tmsu' program name is NOT patched to an absolute
;; store path here (unlike most of this channel's other CLI-wrapping elisp
;; packages): every call goes through `process-file', which is deliberately
;; TRAMP-transparent -- hardcoding a local store path would break that for
;; remote/TRAMP buffers, whose own host needs its own `tmsu' on $PATH
;; regardless.  `tmsu' is propagated instead, so it's on $PATH for the
;; local case.
(define-public emacs-tmsu
  (package
    (name "emacs-tmsu")
    (version "0.9")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/vifon/tmsu.el")
            (commit "625d01d87f2820f648816f78b0c5bc220f14488b")))
      (sha256 (base32 "1h043myzvk40c3lbykikvhy6zcx5ac8r3jhkrzzhak1ngb5lpa9g"))
      (file-name "emacs-tmsu-checkout")))
    (build-system emacs-build-system)
    (arguments (list #:tests? #f)) ; tmsu-tests.el needs a live `tmsu' binary + a scratch tag database
    (propagated-inputs (list tmsu))
    (home-page "https://github.com/vifon/tmsu.el")
    (synopsis "Dired and Org-mode integration for the TMSU file tagger")
    (description
     "Wraps the @command{tmsu} command-line file-tagging tool
(@pxref{tmsu,,,guix,Guix Reference Manual}) for Emacs: @code{tmsu-dired-edit},
@code{tmsu-dired-query}, and @code{tmsu-dired-tags-add}/@code{-remove}
operate on dired's marked files (tag entry via @code{completing-read-multiple});
@code{tmsu-dired-overlay} shows a file's current tags inline in dired;
@code{ol-tmsu} adds Org-mode links to saved tag queries.")
    (license license:gpl3+)))
