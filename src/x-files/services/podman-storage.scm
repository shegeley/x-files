(define-module (x-files services podman-storage)
  #:use-module ((gnu services) #:select (service-type service-extension))
  #:use-module ((gnu services base) #:select (user-processes-service-type))
  #:use-module ((gnu services shepherd)
                #:select (shepherd-service shepherd-root-service-type))
  #:use-module ((guix modules) #:select (source-module-closure guix-module-name?))
  #:use-module (guix gexp)
  #:export (podman-storage-service-type podman-storage-shepherd-services))

(define (podman-storage-shepherd-services config)
  (let ((directory (assq-ref config 'directory))
        (users (assq-ref config 'users)))
    (with-imported-modules
        (source-module-closure
         '((x-files build podman-storage))
         #:select? (lambda (name)
                     (or (guix-module-name? name) (eq? 'x-files (car name)))))
      (list
       (shepherd-service
        (provision '(podman-storage))
        (requirement '(file-systems user-homes))
        (documentation "Bind Podman graph roots to the mounted OCI data drive.")
        (start #~(lambda ()
                   ((@ (x-files build podman-storage) mount-podman-storage)
                    #$directory '#$users)))
        (stop #~(lambda (_)
                  ((@ (x-files build podman-storage) unmount-podman-storage)
                   #$directory '#$users))))))))

(define podman-storage-service-type
  (service-type
   (name 'podman-storage)
   (extensions
    (list (service-extension shepherd-root-service-type
                             podman-storage-shepherd-services)
          (service-extension user-processes-service-type
                             (const '(podman-storage)))))
   (description
    "Bind existing Podman graph-root paths onto a mounted OCI filesystem.
The alist supplies @code{directory} and @code{users}.  Filesystems and user
homes start first; user processes and containers start after the bindings.
No data migration runs during activation, start, stop or rollback.")))
