(define-module (x-files features nix)
  #:use-module ((guix gexp) #:select (local-file plain-file file-append))
  #:use-module ((rde lib file) #:select (find-file-in-load-path))
  #:use-module ((rde features) #:select (feature))
  #:use-module ((rde features emacs) #:select (rde-elisp-configuration-service))
  #:use-module ((gnu services) #:select (service simple-service))
  #:use-module ((gnu home services) #:select (home-xdg-configuration-files-service-type))
  #:use-module ((gnu services base) #:select (udev-rule
                                              udev-rules-service))
  #:use-module ((gnu services nix) #:select (nix-service-type
                                             nix-configuration))
  #:use-module ((gnu system accounts) #:select (user-account? user-account-name
                                                              user-group? user-group-name))
  #:use-module ((gnu packages package-management) #:select (nix))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-envrc
                                                   emacs-nix-mode
                                                   emacs-consult-recoll))
  #:use-module ((gnu packages search) #:select (recoll-cli))
  #:use-module ((gnu packages tree-sitter) #:select (tree-sitter-nix))
  #:use-module ((x-files packages emacs nix-lsp) #:select (emacs-nix-lsp))
  #:use-module ((x-files packages emacs ob-nix) #:select (emacs-ob-nix))
  #:use-module ((x-files packages nix) #:select (nix-manuals nix-manuals-index))
  #:use-module ((srfi srfi-1) #:select (filter member remove))
  #:use-module ((srfi srfi-13) #:select (string-every string-join string-null?))

  #:export (feature-nix-dev))

(define %nix-kvm-udev-rules
  (udev-rules-service 'nix-kvm-access
                      (udev-rule "99-nix-kvm.rules"
               "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0666\"")))

(define %nix-options
  '(("experimental-features" . ("nix-command" "flakes"))
    ("system-features" . ("kvm" "nixos-test" "benchmark" "big-parallel"))))

;; Restricted daemon-side settings: nix-daemon ignores them with a
;; warning when an untrusted client sends them, so they must never end
;; up in the client-side nix.conf.
(define %nix-daemon-only-options
  '("system-features" "trusted-users" "extra-trusted-users"))

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

(define (client-nix-options options)
  "OPTIONS without daemon-only restricted settings."
  (remove (lambda (option)
            (member (car option) %nix-daemon-only-options string=?))
          options))

(define (nix-trusted-entry->name entry)
  "Serialize a Guix account, group, or Nix user/group name."
  (let ((name (cond ((user-account? entry) (user-account-name entry))
                    ((user-group? entry) (user-group-name entry))
                    (else entry))))
    (unless (and (string? name)
                 (not (string-null? name))
                 (string-every (lambda (char)
                                 (and (not (char-whitespace? char))
                                      (not (char=? char #\#))))
                               name))
      (error "Nix trusted entries require nonempty names without whitespace or #"))
    (if (user-group? entry) (string-append "@" name) name)))

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

(define (nix-ob-service config)
  (rde-elisp-configuration-service
   'ob-nix config
   '((with-eval-after-load 'ob
       (require 'ob-nix)))
   #:elisp-packages (list emacs-ob-nix)))

(define (nix-manual-service config)
  (rde-elisp-configuration-service
   'nix-docs config
   `((load-file
      ,(local-file
        (find-file-in-load-path "x-files/packages/aux/nix-docs/nix-docs.el")))
     (setq nix-docs/directory
           ,(file-append nix-manuals "/share/doc/nix-manuals")
           nix-docs/index-directory
           ,(file-append nix-manuals-index "/share/nix-manuals-recoll")
           nix-docs/program ,(file-append recoll-cli "/bin/recollq"))
     (with-eval-after-load 'treesit
                           (add-to-list 'treesit-extra-load-path
                    ,(file-append tree-sitter-nix "/lib/tree-sitter")))
     (add-hook 'nix-mode-hook (function nix-docs/enable))
     (add-hook 'nix-ts-mode-hook (function nix-docs/enable))
     (with-eval-after-load 'eglot
                           (add-hook 'eglot-managed-mode-hook (function nix-docs/enable)))
     (with-eval-after-load 'lsp-mode
                           (add-hook 'lsp-managed-mode-hook (function nix-docs/enable))))
   #:elisp-packages (list emacs-consult-recoll)))

(define* (feature-nix-dev
          #:key
          (package nix)
          (sandbox? #t)
          (documentation? #t)
          (options %nix-options)
          (extra-config #f)
          (nix-trusted-users #f))
  "Configure Nix from OPTIONS, an alist with string keys and string or string-list values.
Daemon-only restricted settings are kept out of the client nix.conf.
DOCUMENTATION? adds offline HTML manuals, Recoll search and Eldoc excerpts.
EXTRA-CONFIG keeps the legacy daemon configuration override.
NIX-TRUSTED-USERS is #f to preserve the daemon trust policy, or a list of
Guix user-account/user-group records and user/@group strings overriding
OPTIONS and EXTRA-CONFIG. Records select existing accounts; they do not create them.
Trusted users may perform privileged Nix operations."
  (define f-name 'nix-dev)
  (define resolved-options (merge-nix-options options))
  (define trusted-names
    (cond ((not nix-trusted-users) #f)
          ((list? nix-trusted-users) (map nix-trusted-entry->name nix-trusted-users))
          (else (error "nix-trusted-users must be #f or a list of accounts, groups, or names"))))

  (define (get-system-services config)
    (list
     (service nix-service-type
              (nix-configuration
               (package package)
               (sandbox sandbox?)
               (extra-config
                (append (or extra-config (map nix-option->line resolved-options))
                        (if trusted-names
                            (list (string-append
                                   "\n"
                                   (nix-option->line
                                    (cons "trusted-users" trusted-names))))
                            '())))))
     %nix-kvm-udev-rules))

  (define (get-home-services config)
    (append
     (list (simple-service
            'nix-client-config
            home-xdg-configuration-files-service-type
            `(("nix/nix.conf"
               ,(plain-file
                 "nix.conf"
                 (apply string-append
                        (map nix-option->line
                             (client-nix-options resolved-options)))))))
           (nix-lsp-service config)
           (nix-envrc-service config)
           (nix-repl-service config)
           (nix-ob-service config))
     (if documentation? (list (nix-manual-service config)) '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (system-services-getter get-system-services)
   (home-services-getter   get-home-services)))
