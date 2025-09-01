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
          (gopls gopls)
          (go-staticcheck go-staticcheck)
          (godef godef))

  ;; https://jorge.olano.dev/blog/getting-started-with-go-and-emacs
  ;; https://geeksocket.in/posts/emacs-lsp-go/

  (define f-name 'golang-extra)

  (define (get-home-services config)
    "Return home services related to Golang."
    (append

     (list
      (simple-service
       'add-go-home-package
       home-profile-service-type
       (list gopls go-staticcheck))

      (append
       (if (get-value 'emacs-lsp config #f)
           (list
            (rde-elisp-configuration-service
             'emacs-golang-lsp
             config
             `((eval-when-compile
                (require 'go-mode)
                (require 'lsp-mode))

               (with-eval-after-load 'lsp
                 (add-hook 'go-ts-mode-hook 'go-mode) ;; go-mode is much more powerfull than go-ts-mode and has lots of bells and whistles. they are both major
                 (add-hook 'go-ts-mode-hook 'lsp)
                 (defun lsp-go-install-save-hooks ()
                   (add-hook 'before-save-hook #'lsp-format-buffer t t)
                   (add-hook 'before-save-hook #'lsp-organize-imports t t))
                 (add-hook 'go-ts-mode-hook #'lsp-go-install-save-hooks)))))
           '())

       (list
        (rde-elisp-configuration-service
         'emacs-golang-extra
         config
         `((eval-when-compile
            (require 'go-mode)
            (require 'flycheck-mode))

           (with-eval-after-load 'go-mode
             (add-hook 'go-mode-hook 'subword-mode)
             ;; or (go-ts-mode-indent-offset 4) ?
             (add-hook 'go-mode-hook (lambda () (setq tab-width 4)))))))

       (if (get-value 'emacs-flycheck config #f)
           (list
            (rde-elisp-configuration-service
             'emacs-golang-flycheck
             config
             `((eval-when-compile
                (require 'go-mode)
                (require 'lsp-mode)
                (require 'flycheck-mode))
               (with-eval-after-load 'lsp
                 (add-hook 'go-mode-hook
                           (lambda ()
                             (flycheck-add-next-checker 'lsp 'go-staticcheck)))))))
           '()))

      '())))

  (feature
   (name f-name)
   (values `((go-lsp? . #t)
             (gopls . ,gopls)
             (go-staticcheck . ,go-staticcheck)))
   (home-services-getter get-home-services)))
