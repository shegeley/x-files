(define-module (x-files packages emacs bun)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)           #:select (package))
  #:use-module ((guix gexp)               #:select (local-file gexp))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((rde lib file)            #:select (find-file-in-load-path))
  #:use-module ((gnu packages text-editors) #:select (rassumfrassum))
  #:use-module ((x-files packages bun)        #:select (bun))
  #:use-module ((x-files packages biome)      #:select (biome))
  #:use-module ((x-files packages typescript) #:select (typescript-native))

  #:export (emacs-bun-mode))

(define emacs-bun-mode
  (package
    (name "emacs-bun-mode")
    (version "0.1.0")
    ;; Bundled in the channel.  #:recursive? #t: patch-exe-paths edits
    ;; bun-mode.el in place, so it needs a writable directory copy.
    (source (local-file
             (dirname (find-file-in-load-path
                       "x-files/packages/aux/bun-mode/bun-mode.el"))
             "emacs-bun-mode-checkout"
             #:recursive? #t))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f
      ;; The rass preset is loaded from next to bun-mode.el.
      #:include #~(cons "\\.py$" %default-include)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-exe-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (emacs-substitute-variables "bun-mode.el"
                ("bun/bin"       (search-input-file inputs "/bin/bun"))
                ("bun/tsc-bin"   (search-input-file inputs "/bin/tsc"))
                ("bun/biome-bin" (search-input-file inputs "/bin/biome"))
                ("bun/rass-bin"  (search-input-file inputs "/bin/rass"))))))))
    (inputs (list bun typescript-native biome rassumfrassum))
    (home-page "https://grigory.tech")
    (synopsis "Emacs major modes, LSP and commands for Bun projects")
    (description
     "Provides @code{bun/ts-mode}, @code{bun/tsx-mode}, @code{bun/js-mode} and
@code{bun/jsx-mode} for files inside Bun projects, an eglot server combining
the native TypeScript language server with Biome via rassumfrassum, and
commands for @command{bun run}, @command{bun test}, @command{bun install},
@command{bun add} and @command{bun x}.  Executable paths are patched to their
Guix store paths at build time.")
    (license license:gpl3+)))
