(define-module (x-files features kuber)
  #:use-module (rde features)
  #:use-module (gnu packages emacs-xyz)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (rde features emacs)

  #:use-module (rde home services shells)

  #:use-module ((x-files packages kuber) #:select (helm kubectl))

  #:use-module ((x-files services kuber) #:select (k0s-worker-service-type
                                                   k0s-worker-configuration
                                                   k0s-controller-service-type
                                                   k0s-controller-configuration))

  #:export (feature-kuber))

(define-public (emacs-kuber config)
  (if
   (get-value 'emacs config #f)
   (list
    (rde-elisp-configuration-service 'kuber config `()
                                     #:summary "Emacs configuration to control kuber"
                                     #:commentary ""
                                     #:keywords '(k0s k8s kuber docker kubernetes system services control)
                                     #:elisp-packages (list emacs-kubed)))
   '()))

(define zsh:k0s-kubectl-alias
  (simple-service 'kubectl-k0s-alias home-zsh-service-type
                  (home-zsh-extension (zshrc (list "alias kubectl=\"k0s kubectl\"")))))

(define* (feature-kuber
          #:key
          (kubectl kubectl)
          (k0s-controller-config (k0s-controller-configuration
                                  (extra-arguments '("--enable-worker"))))
          (k0s-worker-config (k0s-worker-configuration))
          (kubectl-alias? #t)
          (helm helm))

  (define (get-home-services config)
    (append (emacs-kuber config)
            (list
             (when (and (get-value 'zsh config #f)
                        kubectl-alias?)
               zsh:k0s-kubectl-alias)
             (simple-service 'packages-for-kuber
                             home-profile-service-type (list helm)))))

  (define (get-system-services config)
    (list
     (simple-service 'packages-for-kuber
                     profile-service-type (list kubectl))
     (service k0s-controller-service-type k0s-controller-config)
     (service k0s-worker-service-type k0s-worker-config)))

  (feature
   (name 'kuber)
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)
   (values `((kuber? . #t)
             (kuber-provider . k0s)))))
