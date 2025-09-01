(define-module (x-files features golang)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-apps)
  #:use-module (gnu packages golang-check)

  #:use-module (guix gexp)

  #:export (feature-go-extra))

(define* (feature-go-extra
          #:key
          (gopls gopls))

  ;; https://jorge.olano.dev/blog/getting-started-with-go-and-emacs
  ;; https://geeksocket.in/posts/emacs-lsp-go/

  (define f-name 'golang-extra)

  (define (get-home-services config)
    "Return home services related to Golang."

    (require-value 'emacs-lsp config)

    (list
     (simple-service 'add-go-home-package home-profile-service-type
                     (list gopls))
     (rde-elisp-configuration-service
      'emacs-golang-extra config
      `((add-hook 'before-save-hook 'gofmt-before-save)
        (add-hook 'go-ts-mode-hook 'go-mode)
        (add-hook 'go-mode-hook
                  (lambda ()
                    (setq tab-width 4)
                    (subword-mode)
                    (lsp)))))))
  (feature
   (name f-name)
   (values `((go-lsp? . #t)
             (gopls . ,gopls)))
   (home-services-getter get-home-services)))
