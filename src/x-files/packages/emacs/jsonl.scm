(define-module (x-files packages emacs jsonl)
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module ((guix packages)            #:select (package))
  #:use-module ((guix gexp)                #:select (local-file gexp))
  #:use-module ((guix build-system emacs)  #:select (emacs-build-system))
  #:use-module ((gnu packages emacs-xyz)   #:select (emacs-jq-mode))
  #:use-module ((gnu packages web)         #:select (jq)))

(define (aux-directory)
  "Locate the bundled jsonl-mode Emacs source shipped inside this channel.
Searches %load-path the same way the channel's patches are found, so it
resolves both under `-L src' locally and from a `guix pull'ed channel."
  (let loop ((dirs %load-path))
    (if (null? dirs)
        (error "jsonl-mode source not found on %load-path")
        (let ((candidate (string-append (car dirs)
                                        "/x-files/packages/aux/jsonl-mode")))
          (if (file-exists? candidate)
              candidate
              (loop (cdr dirs)))))))

(define-public emacs-jsonl-mode
  (package
    (name "emacs-jsonl-mode")
    (version "0.1.0")
    ;; Bundled in the channel rather than fetched from a separate repo.
    (source (local-file (aux-directory) "emacs-jsonl-mode-checkout"
                        #:recursive? #t))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f                       ; ERT tests pass via `make test' locally
      #:phases
      #~(modify-phases %standard-phases
          ;; Bake the absolute `jq' store path into `jsonl-jq-command' so the
          ;; one-shot `jsonl-jq' (and, via the mode, jq-mode's live preview)
          ;; never rely on $PATH.
          (add-after 'unpack 'patch-jq-path
            (lambda* (#:key inputs #:allow-other-keys)
              (emacs-substitute-variables "jsonl-mode.el"
                ("jsonl-jq-command" (search-input-file inputs "/bin/jq"))))))))
    (inputs (list jq))
    ;; jq-mode is a hard dependency (see Package-Requires); propagate it so it
    ;; lands in the same profile and `jsonl-jq-interactively' works.
    (propagated-inputs (list emacs-jq-mode))
    (home-page "https://github.com/shegeley/x-files")
    (synopsis "Major mode for JSON Lines (JSONL / NDJSON) files")
    (description
     "@code{jsonl-mode} is an Emacs major mode for JSON Lines files (also known
as JSONL or NDJSON), a format where every line is an independent, complete JSON
value.  It is derived from the built-in @code{js-json-mode}, inheriting JSON
font-lock and indentation, and adds semantics specific to the one-record-per-line
format: record navigation, a count command, per-line validation via a Flymake
backend, pretty-printing a single record, and filtering records through jq (both
a one-shot command and a live preview built on @code{jq-mode}, each preserving
the JSON Lines shape).  Files ending in @file{.jsonl} or @file{.ndjson} are
handled automatically.")
    (license license:gpl3+)))
