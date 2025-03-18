(define-module (x-files features zellij)
  ;; note: i don't use it anymore. only emacs for terminal multiplexing
  #:use-module ((rde features) #:select (feature))
  #:use-module ((gnu services) #:select (simple-service))
  #:use-module (gnu home services)
  #:use-module ((guix gexp) #:select (mixed-text-file))
  #:use-module (x-files packages zellij)

  #:export (feature-zellij))

(define zellij-default-conf-file ;; kdl https://kdl.dev/
  (mixed-text-file "zellij"
                   "copy_command \"wl-copy\" " ;; for `gnome-terminal'
                   ;; "\n"
                   ;; "theme \"catppuccin-frappe\""
                   ))

(define* (feature-zellij
          #:key
          (zellij zellij)
          (auto-attach? #f)
          (zellij-kdl zellij-default-conf-file))

  "I don't use it anymore, only emacs built-in terminals.
   Decided to publish for new guix'ers as example"

  (define (zellij-home-services config)
    (list
     (when auto-attach?
       (simple-service 'zellij-auto-attach-env
                        home-environment-variables-service-type
                        `(("ZELLIJ_AUTO_ATTACH" . "true"))))

     (simple-service 'zellij-config
                     home-xdg-configuration-files-service-type
                     `(("zellij" ,zellij-kdl)))

     (simple-service 'add-zellij-packages -profile-service-type
                     (list zellij))))

  (feature
   (name 'zsh)
   (home-services-getter zellij-home-services)))
