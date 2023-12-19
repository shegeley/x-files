(define-module (x-files services docker)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services docker)
  #:use-module (guix gexp)

  #:use-module (gnu packages docker)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages guile)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)

  #:use-module (gnu packages docker)

  #:export (docker-configuration
            docker-service-type))

(define default-data-directory
  "/var/lib/docker")

(define daemon-configuration
  `(("data-root" . ,default-data-directory)))

(define-maybe file-like)

(define-configuration docker-configuration
  (docker
   (file-like docker)
   "Docker daemon package.")
  (docker-compose
   (file-like docker-compose)
   "Docker-compose package.")
  (docker-cli
   (file-like docker-cli)
   "Docker client package.")
  (containerd
   (file-like containerd)
   "containerd package.")
  (key
   maybe-file-like
   "/etc/docker/key.json")
  (daemon-configuration
   (list daemon-configuration)
   "/etc/docker/daemon.json")
  (environment-variables
   (list '())
   "Environment variables to set for dockerd")
  (no-serialization))

(define %docker-accounts
  (list
   (user-group (name "docker")
               (system? #t))))

(define (%containerd-activation config)
  (let ((data-root
         (or
          (assoc-ref (docker-configuration-daemon-configuration config)
                     "data-root")
          "/var/lib/docker")))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$data-root))))

(define %docker-activation %containerd-activation)

(define (docker-etc configuration)
  `(("docker/daemon.json"
     ,(with-extensions (list guile-json-4)
        (with-imported-modules (source-module-closure `((json)))
          (computed-file
           "daemon.json"
           #~(begin
               (use-modules (json))
               (call-with-output-file #$output
                 (lambda (port)
                   (scm->json '#$(docker-configuration-daemon-configuration configuration)
                              port))))))))
    ("docker/key.json"
     ,(docker-configuration-key configuration))))

(define (containerd-shepherd-service config)
  (let* ((package (docker-configuration-containerd config))
         (containerd (docker-configuration-containerd config)))
    (shepherd-service
     (documentation "containerd daemon.")
     (provision '(containerd))
     (start #~(make-forkexec-constructor
               (list (string-append #$package "/bin/containerd"))
               ;; For finding containerd-shim binary.
               #:environment-variables
               (list (string-append "PATH=" #$containerd "/bin"))
               #:pid-file "/run/containerd/containerd.pid"
               #:pid-file-timeout 300
               #:log-file "/var/log/containerd.log"))
     (stop #~(make-kill-destructor)))))

(define (docker-shepherd-service config)
  (let* ((docker (docker-configuration-docker config))
         (environment-variables (docker-configuration-environment-variables config)))
    (shepherd-service
     (documentation "Docker daemon.")
     (provision '(dockerd))
     (requirement '(containerd
                    dbus-system
                    elogind
                    file-system-/sys/fs/cgroup
                    networking
                    udev))
     (start #~(make-forkexec-constructor
               (list (string-append #$docker "/bin/dockerd")
                     "-p" "/var/run/docker.pid"
                     "--containerd" "/run/containerd/containerd.sock")
               #:environment-variables
               (list #$@environment-variables)
               #:pid-file "/var/run/docker.pid"
               #:log-file "/var/log/docker.log"))
     (stop #~(make-kill-destructor)))))

(define docker-service-type
  (service-type
   (name 'docker)
   (description "Provide capability to run Docker application
bundles in Docker containers.")
   (extensions
    (list
     ;; Make sure the 'docker' command is available.
     (service-extension etc-service-type
                        docker-etc)
     (service-extension profile-service-type
                        (lambda (config)
                          (list
                           (docker-configuration-docker-cli config)
                           (docker-configuration-docker-compose config))))
     (service-extension activation-service-type
                        %docker-activation)
     (service-extension shepherd-root-service-type
                        (lambda (config)
                          (list (containerd-shepherd-service config)
                                (docker-shepherd-service config))))
     (service-extension account-service-type
                        (const %docker-accounts))))
   (default-value (docker-configuration))))
