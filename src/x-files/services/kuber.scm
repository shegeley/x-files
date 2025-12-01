(define-module (x-files services kuber)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (x-files packages kuber)
  #:use-module (srfi srfi-1)

  #:export (k0s-controller-configuration
            k0s-controller-configuration?
            k0s-controller-service-type
            k0s-worker-configuration
            k0s-worker-configuration?
            k0s-worker-service-type))

;; https://git.sr.ht/~abcdw/cons.town/blob/master/src/guile/cons/guix/system/services/kubernetes.scm

(define %default-system-environment
  (list
   "PATH=/run/privileged/bin:\
/run/current-system/profile/bin:/run/current-system/profile/sbin"))

(define-record-type* <k0s-controller-configuration>
  k0s-controller-configuration make-k0s-controller-configuration
  k0s-controller-configuration?
  (k0s k0s-controller-configuration-k0s (default k0s))
  (log-file k0s-controller-log-file (default "/var/log/k0s-controller"))
  (extra-arguments k0s-controller-configuration-extra-arguments (default '())))

(define (k0s-controller-shepherd-service config)
  (list
   (shepherd-service
     (provision '(k0s-controller))
     ;; depends file-system-/sys/fs/cgroup
     ;; by-default it's fs comes with elogind on graphical environments
     (requirement '(file-systems))
     (documentation "Run k0s controller.")
     (start
      #~(make-forkexec-constructor
         (list
          #$(file-append (k0s-controller-configuration-k0s config)
                         "/bin/k0s")
          "controller"
          #$@(k0s-controller-configuration-extra-arguments config))
         #:environment-variables '#$%default-system-environment
         #:log-file #$(k0s-controller-log-file config)))
     (auto-start? #t)
     (stop #~(make-kill-destructor)))))

(define k0s-controller-service-type
  (service-type
    (name 'k0s-controller)
    (extensions
     (list
      (service-extension profile-service-type
                         (lambda (config)
                           (list (k0s-controller-configuration-k0s config))))
      (service-extension shepherd-root-service-type
                         k0s-controller-shepherd-service)))
    (default-value (k0s-controller-configuration))
    (description "Run the k0s controller.")))

(define-record-type* <k0s-worker-configuration>
  k0s-worker-configuration make-k0s-worker-configuration
  k0s-worker-configuration?
  (k0s       k0s-worker-configuration-k0s (default k0s))
  (log-file  k0s-worker-log-file (default "/var/log/k0s-worker"))
  (token-file k0s-worker-configuration-token-file
              (default "/var/lib/k0s/worker-token"))
  (extra-arguments k0s-worker-configuration-extra-arguments
                   (default '())))

(define (k0s-worker-shepherd-service config)
  (list
   (shepherd-service
     (provision '(k0s-worker))
     ;; depends file-system-/sys/fs/cgroup
     ;; by-default it's fs comes with elogind on graphical environments
     (requirement '(file-systems))
     (documentation "Run k0s worker.")
     (start
      #~(make-forkexec-constructor
         (list
          #$(file-append (k0s-worker-configuration-k0s config)
                         "/bin/k0s")
          "worker"
          "--token-file"
          #$(k0s-worker-configuration-token-file config)
          #$@(k0s-worker-configuration-extra-arguments config))
         #:environment-variables '#$%default-system-environment
         #:log-file #$(k0s-worker-log-file config)))
     (auto-start? #t)
     (stop #~(make-kill-destructor)))))

(define k0s-worker-service-type
  (service-type
    (name 'k0s-worker)
    (extensions
     (list
      (service-extension profile-service-type
                         (lambda (config)
                           (list (k0s-worker-configuration-k0s config))))
      (service-extension shepherd-root-service-type
                         k0s-worker-shepherd-service)))
    (default-value (k0s-worker-configuration))
    (description "Run the k0s worker.")))
