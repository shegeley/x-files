(define-module (x-files features emacs lsp)
  #:use-module (rde features)
  #:use-module (rde features emacs)

  #:use-module ((gnu packages emacs-xyz) #:select (emacs-lsp-mode
                                                   emacs-lsp-ui))

  #:use-module ((guix transformations) #:select (options->transformation))

  #:export (feature-emacs-lsp))

(define (el-mock-fix-transformation package)
  ;; el-mock is broken on current commits at the time of wrtiting this feature
  ((options->transformation '((without-tests . "emacs-el-mock")))
    package))

(define* (feature-emacs-lsp
          #:key
          (emacs-lsp-mode (el-mock-fix-transformation emacs-lsp-mode))
          (emacs-lsp-ui   (el-mock-fix-transformation emacs-lsp-ui)))

  (define f-name 'emacs-lsp)
  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      f-name config `()
      #:elisp-packages (list emacs-lsp-mode emacs-lsp-ui))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-lsp-mode . ,emacs-lsp-mode)
             (emacs-lsp-ui . ,emacs-lsp-ui)))
   (home-services-getter get-home-services)))
