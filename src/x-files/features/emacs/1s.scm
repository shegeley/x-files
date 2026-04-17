(define-module (x-files features emacs 1s)
  #:use-module ((guix gexp)          #:select (file-append))
  #:use-module ((rde features)       #:select (feature get-value))
  #:use-module ((rde features emacs) #:select (rde-elisp-configuration-service))
  #:use-module ((gnu services)       #:select (simple-service))
  #:use-module ((gnu home services)  #:select (home-profile-service-type))
  #:use-module ((gnu packages java)  #:select (openjdk))
  #:use-module ((x-files packages 1s)       #:select (bsl-language-server))
  #:use-module ((x-files packages emacs 1s) #:select (emacs-1s))

  #:export (feature-emacs-1s))

(define* (feature-emacs-1s
          #:key
          (bsl-language-server bsl-language-server)
          (emacs-1s emacs-1s)
          (openjdk openjdk))

  (define java-exe (file-append openjdk "/bin/java"))
  (define server-jar
    (file-append bsl-language-server
                 "/lib/bsl-language-server/bsl-language-server.jar"))

  (define (emacs-config config)
    (rde-elisp-configuration-service
     'emacs-1s
     config
     `((require '1s-mode)
       (require 'bsl-ls)

       (setq bsl-ls-java      ,java-exe
             bsl-ls-server-jar ,server-jar)

       (with-eval-after-load 'eglot
         (add-hook '1s-mode-hook 'eglot-ensure)
         (add-to-list 'eglot-server-programs
                      '(1s-mode . bsl-ls-contact))))
     #:elisp-packages (list emacs-1s)))

  (define (get-home-services config)
    (list
     (when (get-value 'emacs config) (emacs-config config))
     (simple-service
      '1s-add-packages
      home-profile-service-type
      (list bsl-language-server))))

  (feature
   (name 'emacs-1s)
   (values `((emacs-1s . #t)))
   (home-services-getter get-home-services)))
