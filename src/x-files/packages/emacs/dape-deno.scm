(define-module (x-files packages emacs dape-deno)
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module ((guix packages)            #:select (package))
  #:use-module ((guix gexp)                #:select (local-file gexp))
  #:use-module ((guix build-system emacs)  #:select (emacs-build-system))
  #:use-module ((rde lib file)             #:select (find-file-in-load-path))
  #:use-module ((gnu packages emacs-xyz)   #:select (emacs-dape))
  #:use-module ((x-files packages deno)             #:select (deno))
  #:use-module ((x-files packages vscode-js-debug)  #:select (node-vscode-js-debug-latest))

  #:export (emacs-dape-deno))

(define emacs-dape-deno
  (package
    (name "emacs-dape-deno")
    (version "0.1.0")
    ;; Bundled in the channel rather than fetched from a separate repo.
    ;; #:recursive? #t: patch-exe-paths below edits dape-deno.el in place, so
    ;; it needs the directory-unpack's writable copy, not a bare read-only
    ;; single-file local-file.
    (source (local-file
             (dirname (find-file-in-load-path "x-files/packages/aux/dape-deno/dape-deno.el"))
             "emacs-dape-deno-checkout"
             #:recursive? #t))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Bake the absolute `dapDebugServer'/`deno' store paths into
          ;; dape-deno.el so its dape-configs never rely on $PATH.
          (add-after 'unpack 'patch-exe-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (emacs-substitute-variables "dape-deno.el"
                ("dape-deno-debug-exe"
                 (search-input-file inputs "/bin/dapDebugServer"))
                ("dape-deno-deno-exe"
                 (search-input-file inputs "/bin/deno"))))))))
    (inputs (list node-vscode-js-debug-latest deno))
    (propagated-inputs (list emacs-dape))
    (home-page "https://grigory.tech")
    (synopsis "dape debug configurations for Deno")
    (description
     "Registers @code{deno}, @code{chrome-frontend}, and @code{deno-attach}
@code{dape} debug configurations for @code{deno-mode} buffers, backed by
vscode-js-debug's @code{dapDebugServer}.  @code{dape-deno-debug-exe} and
@code{dape-deno-deno-exe} are patched to their absolute Guix store paths at
build time.")
    (license license:gpl3+)))
