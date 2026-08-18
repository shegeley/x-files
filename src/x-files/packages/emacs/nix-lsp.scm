(define-module (x-files packages emacs nix-lsp)
  #:use-module ((guix packages)           #:select (package))
  #:use-module ((guix gexp)               #:select (local-file gexp))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((gnu packages emacs-xyz)  #:select (emacs-nix-mode))
  #:use-module ((x-files packages nixd)   #:select (nixd))

  #:export (emacs-nix-lsp))

(define (aux-directory)
  "Locate the bundled nix-lsp.el shipped inside this channel.  Searches
%load-path the same way (x-files packages emacs jsonl)'s jsonl-mode source is
found, so it resolves both under `-L src' locally and from a `guix pull'ed
channel."
  (let loop ((dirs %load-path))
    (if (null? dirs)
        (error "nix-lsp.el source not found on %load-path")
        (let ((candidate (string-append (car dirs)
                                        "/x-files/packages/aux/nix-lsp")))
          (if (file-exists? candidate)
              candidate
              (loop (cdr dirs)))))))

(define emacs-nix-lsp
  (package
    (name "emacs-nix-lsp")
    (version "0.1.0")
    ;; Bundled in the channel rather than fetched from a separate repo.
    (source (local-file (aux-directory) "emacs-nix-lsp-checkout"
                        #:recursive? #t))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Bake the absolute `nixd' store path into nix-lsp.el so its
          ;; lsp-mode/eglot wiring never relies on $PATH.
          (add-after 'unpack 'patch-nixd-path
            (lambda* (#:key inputs #:allow-other-keys)
              (emacs-substitute-variables "nix-lsp.el"
                ("nix-lsp-nixd-exe"
                 (search-input-file inputs "/bin/nixd"))))))))
    (inputs (list nixd))
    (propagated-inputs (list emacs-nix-mode))
    (home-page "https://grigory.tech")
    (synopsis "nixd wiring for lsp-mode and eglot")
    (description
     "Points lsp-mode's built-in nix-mode/nix-ts-mode client
(@code{lsp-nix-nixd-server-path}) and eglot's @code{eglot-server-programs}
nix-mode entry at nixd, the evaluation-backed Nix language server.
@code{nix-lsp-nixd-exe} is patched to its absolute Guix store path at build
time.")
    (license license:gpl3+)))
