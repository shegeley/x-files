(define-module (x-files tests services podman-storage)
  #:use-module ((gnu tests) #:select (simple-operating-system marionette-operating-system system-test))
  #:use-module ((gnu system) #:select (operating-system operating-system-file-systems))
  #:use-module ((gnu system file-systems) #:select (file-system))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((gnu services) #:select (simple-service))
  #:use-module ((gnu services shepherd) #:select (shepherd-root-service-type shepherd-service-file))
  #:use-module ((guix modules) #:select (source-module-closure guix-module-name?))
  #:use-module ((guix scripts system reconfigure) #:select (upgrade-services-program))
  #:use-module ((x-files services podman-storage) #:select (podman-storage-shepherd-services))
  #:use-module (guix gexp)
  #:export (%test-podman-storage))

(define %config '((directory . "/oci") (users . ("root" "alice"))))
(define %services (podman-storage-shepherd-services %config))

(define (run-test)
  (define base
    (simple-operating-system
     ;; Leave user-processes independent so tests can stop storage without
     ;; killing the marionette. Production explicitly orders it after storage.
     (simple-service 'test-storage shepherd-root-service-type %services)))
  (define os
    (marionette-operating-system
     (operating-system
      (inherit base)
      (file-systems
       (cons (file-system (device "none") (mount-point "/oci")
                          (type "tmpfs") (check? #f))
             (operating-system-file-systems base))))
     #:imported-modules
     (source-module-closure
      '((gnu services herd) (x-files build podman-storage))
      #:select? (lambda (name)
                  (or (guix-module-name? name) (eq? 'x-files (car name)))))))
  (define vm (virtual-machine (operating-system os) (memory-size 1024)))
  (define disable (upgrade-services-program '() '() '(podman-storage) '()))
  (define enable
    (upgrade-services-program (map shepherd-service-file %services)
                              '(podman-storage) '() '()))
  (gexp->derivation
   "podman-storage-lifecycle"
   (with-imported-modules '((gnu build marionette))
     #~(begin
         (use-modules ((gnu build marionette) #:select (make-marionette marionette-eval system-test-runner))
                      ((srfi srfi-64) #:select (test-runner-current test-begin test-assert test-end)))
         (define marionette (make-marionette (list #$vm)))
         (define (guest expression) (marionette-eval expression marionette))
         (define (action verb)
           (guest `(begin
                     (use-modules ((gnu services herd) #:select (with-shepherd-action)))
                     (with-shepherd-action 'podman-storage (',verb) result #t))))
         (define (mounted? path)
           (guest `(begin
                     (use-modules ((guix build syscalls) #:select (mount-points umount)))
                     (and (member ,path (mount-points)) #t))))
         (define (stores?)
           (and (mounted? "/var/lib/containers/storage")
                (mounted? "/home/alice/.local/share/containers/storage")))
         (define (switch program) (guest `(begin (primitive-load ,program) #t)))
         (test-runner-current (system-test-runner #$output))
         (test-begin "podman-storage-lifecycle")
         (test-assert "initial start after home and backing filesystem"
           (and (guest '(begin
                          (use-modules ((gnu services herd) #:select (wait-for-service)))
                          (wait-for-service 'podman-storage) #t))
                (stores?)))
         (test-assert "rootless directory ownership is correct"
           (guest '(= (passwd:uid (getpwnam "alice"))
                      (stat:uid (stat "/oci/podman/alice/storage"))
                      (stat:uid (stat "/home/alice/.local/share/containers/storage")))))
         (test-assert "writes through original graph root reach OCI drive"
           (guest '(begin
                     (call-with-output-file "/var/lib/containers/storage/preserved"
                       (lambda (port) (display "image data" port)))
                     (file-exists? "/oci/podman/root/storage/preserved"))))
         (test-assert "repeated start does not stack mounts"
           (guest '(begin
                     (use-modules ((x-files build podman-storage) #:select (mount-podman-storage))
                                  ((guix build syscalls) #:select (mount-points)))
                     (let ((before (mount-points)))
                       (and (mount-podman-storage "/oci" '("root" "alice"))
                            (equal? before (mount-points)))))))
         (test-assert "stop removes bindings but preserves image data"
           (and (action 'stop) (not (stores?))
                (guest '(file-exists? "/oci/podman/root/storage/preserved"))))
         (test-assert "second start succeeds"
           (and (action 'start) (stores?)))
         (test-assert "cleanup tolerates externally unmounted storage"
           (and (guest '(begin (umount "/var/lib/containers/storage") #t))
                (action 'stop) (not (stores?))))
         (test-assert "already stopped cleanup is harmless"
           (guest '(begin
                     (use-modules ((x-files build podman-storage) #:select (unmount-podman-storage)))
                     (not (unmount-podman-storage "/oci" '("root" "alice"))))))
         (test-assert "a populated graph root is never hidden"
           (guest '(begin
                     (call-with-output-file "/var/lib/containers/storage/local-data"
                       (lambda (port) (display "keep" port)))
                     (and (not (mount-podman-storage "/oci" '("root" "alice")))
                          (file-exists? "/var/lib/containers/storage/local-data")
                          (begin (delete-file "/var/lib/containers/storage/local-data") #t)))))
         (test-assert "a later preparation failure cleans up earlier bindings"
           (guest '(begin
                     (rmdir "/home/alice/.local/share/containers/storage")
                     ;; A dangling child lets preflight pass but mkdir fail.
                     (rmdir "/home/alice/.local/share/containers")
                     (call-with-output-file "/home/alice/.local/share/containers"
                       (lambda (port) (display "block" port)))
                     (and (not (mount-podman-storage "/oci" '("root" "alice")))
                          (not (member "/var/lib/containers/storage" (mount-points)))
                          (begin (delete-file "/home/alice/.local/share/containers") #t)))))
         (test-assert "missing OCI mount never falls back to the root filesystem"
           (guest '(not (mount-podman-storage "/absent-oci" '("root" "alice")))))
         (test-assert "retry after failure succeeds"
           (and (action 'start) (stores?)))
         (test-assert "generation switch removes bindings"
           (and (switch #$disable) (not (stores?))))
         (test-assert "rollback restores bindings and data"
           (and (switch #$enable) (stores?)
                (guest '(file-exists? "/var/lib/containers/storage/preserved"))))
         (test-end)))))

(define %test-podman-storage
  (system-test
   (name "podman-storage-lifecycle")
   (description "Exercise OCI bindings, ownership, refusal to hide data, failure cleanup and generation rollback in a VM.")
   (value (run-test))))
