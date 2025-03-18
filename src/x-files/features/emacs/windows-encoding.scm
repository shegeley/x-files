(define-module (x-files features emacs windows-encoding)
  #:use-module (rde features)
  #:use-module (rde features emacs)

  #:export (feature-emacs-windows-encoding))

(define* (feature-emacs-windows-encoding)
  "Add and configure emacs-windows-encoding ­ to hide annoying ^m characters (from windows system)"
  (define f-name 'emacs-windows-encoding)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((setq buffer-display-table (make-display-table))
        (aset buffer-display-table ?\^m []))
      #:elisp-packages '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
