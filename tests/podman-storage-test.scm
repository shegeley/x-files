(use-modules ((ares suitbl) #:select (suite test is current-test-runner get-state make-suitbl))
             ((ares suitbl state) #:select (get-run-history))
             ((srfi srfi-1) #:select (find every))
             ((rde features) #:select (feature rde-config feature-system-services-getter))
             ((gnu services) #:select (service-kind service-value))
             ((gnu system file-systems) #:select (file-system))
             ((x-files features oci-via-podman) #:select (feature-oci-via-podman))
             ((x-files services podman-storage) #:select (podman-storage-service-type)))

(define* (binding file-systems #:optional (mount-point "/oci"))
  (find (lambda (service) (eq? (service-kind service) podman-storage-service-type))
        ((feature-system-services-getter
          (feature-oci-via-podman #:storage-mount-point mount-point))
         (rde-config
          (features
           (list (feature
                  (name 'test-values)
                  (values `((user-name . "alice") (file-systems . ,file-systems))))))))))

(define* (filesystem point #:optional (mount? #t))
  (file-system (device "test") (mount-point point) (type "btrfs") (mount? mount?)))

(define runner (make-suitbl))
(current-test-runner runner)
(suite "Registry-selected Podman storage"
  (test "Hosts without the selected filesystem retain their storage" ()
    (is (not (binding '())))
    (is (not (binding (list (filesystem "/data"))))))
  (test "A registered OCI filesystem binds every Podman account" ()
    (is (equal? '((directory . "/oci") (users . ("root" "oci-container" "alice")))
                (service-value (binding (list (filesystem "/oci")))))))
  (test "Unmounted registry filesystems do not enable binding" ()
    (is (not (binding (list (filesystem "/oci" #f))))))
  (test "An explicit alternative mount is supported" ()
    (is (equal? "/containers"
                (assq-ref (service-value
                           (binding (list (filesystem "/containers")) "/containers"))
                          'directory))))
  (test "Binding can be explicitly disabled" ()
    (is (not (binding (list (filesystem "/oci")) #f)))))

(let ((history (get-run-history (get-state runner))))
  (exit (if (and (pair? history)
                 (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome))) history))
            0 1)))
