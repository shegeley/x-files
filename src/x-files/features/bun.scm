(define-module (x-files features bun)
  #:use-module ((guix gexp) #:select (file-append))

  #:use-module ((rde features) #:select (feature get-value))
  #:use-module ((rde features emacs) #:select (rde-elisp-configuration-service))

  #:use-module ((gnu services) #:select (simple-service))
  #:use-module ((gnu home services) #:select (home-profile-service-type))

  #:use-module ((gnu packages text-editors) #:select (rassumfrassum))
  #:use-module ((gnu packages tree-sitter) #:select (tree-sitter-typescript
                                                     tree-sitter-javascript
                                                     tree-sitter-jsdoc))

  #:use-module ((x-files packages bun) #:select ((bun . default-bun)))
  #:use-module ((x-files packages biome) #:select ((biome . default-biome)))
  #:use-module ((x-files packages typescript) #:select (typescript-native))
  #:use-module ((x-files packages emacs bun) #:select ((emacs-bun-mode . default-emacs-bun-mode)))

  #:export (feature-bun))

(define* (feature-bun
          #:key
          (bun default-bun)
          (biome default-biome)
          (typescript typescript-native)
          (rass rassumfrassum)
          (emacs-bun-mode default-emacs-bun-mode))
  "Bun runtime plus editor tooling Bun itself lacks: the native TypeScript
language server (types) and Biome (lint, format), combined into one eglot
server by rass.  Emacs parts apply only to files inside Bun projects; see
@code{bun-project-markers} in @file{bun-mode.el}."

  (define f-name 'bun)

  (define (emacs-config config)
    (rde-elisp-configuration-service
     f-name config
     `((require 'bun-mode)
       ;; The feature's packages win over the ones baked into emacs-bun-mode.
       (setq bun-bin       ,(file-append bun "/bin/bun")
             bun-tsc-bin   ,(file-append typescript "/bin/tsc")
             bun-biome-bin ,(file-append biome "/bin/biome")
             bun-rass-bin  ,(file-append rass "/bin/rass"))
       ,@(if (get-value 'emacs-eglot config #f)
             `((dolist (hook '(bun-ts-mode-hook
                               bun-tsx-mode-hook
                               bun-js-mode-hook
                               bun-jsx-mode-hook))
                 (add-hook hook (function eglot-ensure))))
             '()))
     #:summary "Bun projects: bun-*-modes, native TypeScript + Biome via eglot"
     #:elisp-packages (list emacs-bun-mode)))

  (define (get-home-services config)
    (list
     (when (get-value 'emacs config #f) (emacs-config config))
     (simple-service
      'bun-add-packages
      home-profile-service-type
      ;; js-ts-mode (Emacs 31+) also loads the jsdoc grammar.
      (list bun biome typescript
            tree-sitter-typescript tree-sitter-javascript tree-sitter-jsdoc))))

  (feature
   (name f-name)
   (values `((,f-name . ,bun)
             (biome . ,biome)
             (emacs-bun . ,emacs-bun-mode)))
   (home-services-getter get-home-services)))
