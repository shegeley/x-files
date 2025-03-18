(define-module (x-files features gdm)
  #:use-module (gnu)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module ((gnu packages gnome) #:select (gdm))
  #:use-module ((gnu services xorg) #:select (gdm-service-type gdm-configuration))
  #:export (feature-gdm))

(define gdm-configuration-wayland?
  (@@ (gnu services xorg) gdm-configuration-wayland?))

(define* (feature-gdm
          #:key
          (lang "en_US.UTF-8")
          (configuration (gdm-configuration (wayland? #t))))

  (define (get-system-services config)
    (list
     (simple-service 'global-envvars session-environment-service-type `(("GDM_LANG" . ,lang)))
     (service gdm-service-type
              (gdm-configuration
               (inherit configuration)
               (wayland? #t)))))

  (define wayland? (gdm-configuration-wayland? configuration))

  (feature
   (name 'gdm)
   (values `((desktop-manager . 'gdm)
             (xorg? . ,(not wayland?))
             (wayland? . ,wayland?)))
   (system-services-getter get-system-services)))
