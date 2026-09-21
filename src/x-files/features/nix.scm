(define-module (x-files features nix)
  #:use-module ((guix gexp) #:select (local-file))
  #:use-module ((rde lib file) #:select (find-file-in-load-path))
  #:use-module ((rde features) #:select (feature))
  #:use-module ((rde features emacs) #:select (rde-elisp-configuration-service))
  #:use-module ((gnu services) #:select (service))
  #:use-module ((gnu services base) #:select (udev-rule
                                              udev-rules-service))
  #:use-module ((gnu services nix) #:select (nix-service-type
                                              nix-configuration))
  #:use-module ((gnu packages package-management) #:select (nix))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-envrc
                                                    emacs-nix-mode))
  #:use-module ((x-files packages emacs nix-lsp) #:select (emacs-nix-lsp))

  #:export (feature-nix-dev))

(define %nix-kvm-udev-rules
  (udev-rules-service 'nix-kvm-access
    (udev-rule "99-nix-kvm.rules"
      "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"")))

(define (nix-lsp-service config)
  (rde-elisp-configuration-service
   'nix-lsp config
   '((require 'nix-lsp))
   #:elisp-packages (list emacs-nix-lsp)))

(define (nix-envrc-service config)
  (rde-elisp-configuration-service
   'nix-envrc config
   '((require 'envrc)
     (envrc-global-mode))
   #:elisp-packages (list emacs-envrc)))

(define (nix-repl-service config)
  (rde-elisp-configuration-service
   'nix-repl config
   `((load-file
      ,(local-file
        (find-file-in-load-path
         "x-files/packages/aux/nix-repl/nix-repl-config.el"))))
   #:elisp-packages (list emacs-nix-mode)))

(define* (feature-nix-dev
          #:key
          (package nix)
          (sandbox? #t)

          (extra-config (list "experimental-features = nix-command flakes\n"
                               "system-features = kvm nixos-test benchmark big-parallel\n")))
  (define f-name 'nix-dev)

  (define (get-system-services config)
    (list
     (service nix-service-type
              (nix-configuration
               (package package)
               (sandbox sandbox?)
               (extra-config extra-config)))
     %nix-kvm-udev-rules))

  (define (get-home-services config)
    (list (nix-lsp-service config)
          (nix-envrc-service config)
          (nix-repl-service config)))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (system-services-getter get-system-services)
   (home-services-getter   get-home-services)))
