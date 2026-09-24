(define-module (x-files features nix)
  #:use-module ((guix gexp) #:select (local-file plain-file))
  #:use-module ((rde lib file) #:select (find-file-in-load-path))
  #:use-module ((rde features) #:select (feature))
  #:use-module ((rde features emacs) #:select (rde-elisp-configuration-service))
  #:use-module ((gnu services) #:select (service simple-service))
  #:use-module ((gnu home services) #:select (home-xdg-configuration-files-service-type))
  #:use-module ((gnu services base) #:select (udev-rule
                                              udev-rules-service))
  #:use-module ((gnu services nix) #:select (nix-service-type
                                              nix-configuration))
  #:use-module ((gnu packages package-management) #:select (nix))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-envrc
                                                    emacs-nix-mode))
  #:use-module ((x-files packages emacs nix-lsp) #:select (emacs-nix-lsp))
  #:use-module ((srfi srfi-1) #:select (filter))
  #:use-module ((srfi srfi-13) #:select (string-join))

  #:export (feature-nix-dev))

(define %nix-kvm-udev-rules
  (udev-rules-service 'nix-kvm-access
    (udev-rule "99-nix-kvm.rules"
      "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"")))

(define %nix-options
  '(("experimental-features" . ("nix-command" "flakes"))
    ("system-features" . ("kvm" "nixos-test" "benchmark" "big-parallel"))))

(define (nix-option->line option)
  (string-append (car option)
                 " = "
                 (let ((value (cdr option)))
                   (if (list? value)
                       (string-join value " ")
                       value))
                 "\n"))

(define (merge-nix-options options)
  (append options
          (filter (lambda (default)
                    (not (assoc (car default) options)))
                  %nix-options)))

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
          (options %nix-options)
          (extra-config #f))
  "Configure Nix from OPTIONS, an alist with string keys and string or string-list values.
EXTRA-CONFIG keeps the legacy daemon configuration override."
  (define f-name 'nix-dev)
  (define resolved-options (merge-nix-options options))

  (define (get-system-services config)
    (list
     (service nix-service-type
              (nix-configuration
               (package package)
               (sandbox sandbox?)
               (extra-config (or extra-config
                                 (map nix-option->line resolved-options)))))
     %nix-kvm-udev-rules))

  (define (get-home-services config)
    (list (simple-service 'nix-client-config
                          home-xdg-configuration-files-service-type
          `(("nix/nix.conf"
             ,(plain-file
               "nix.conf"
               (apply string-append
                      (map nix-option->line resolved-options))))))
          (nix-lsp-service config)
          (nix-envrc-service config)
          (nix-repl-service config)))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (system-services-getter get-system-services)
   (home-services-getter   get-home-services)))
