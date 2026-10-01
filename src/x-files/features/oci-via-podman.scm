(define-module (x-files features oci-via-podman)
  #:use-module ((rde features) #:select (feature get-value))
  #:use-module (guix gexp)
  #:use-module ((gnu packages containers) #:select (podman
                                                    podman-compose))
  #:use-module ((gnu services containers) #:select (oci-service-type
                                                    oci-configuration
                                                    rootless-podman-service-type
                                                    rootless-podman-configuration))
  #:use-module ((gnu home services containers) #:select (home-oci-service-type))
  #:use-module ((gnu home services) #:select (home-profile-service-type))
  #:use-module ((gnu packages tree-sitter) #:select (tree-sitter-dockerfile))
  #:use-module ((gnu services) #:select (simple-service service for-home))

  #:use-module ((gnu services networking) #:select (iptables-service-type))
  #:use-module ((gnu system accounts) #:select (user-account
                                                user-group
                                                subid-range))

  #:use-module ((gnu system file-systems)
                #:select (file-system-mount-point file-system-mount?))
  #:use-module ((x-files services podman-storage)
                #:select (podman-storage-service-type))
  #:use-module ((srfi srfi-1) #:select (any))

  #:export (feature-oci-via-podman))

(define storage-drivers
  `((btrfs . ,(plain-file "storage.conf"
                          "[storage]
driver = \"btrfs\"
"))))

(define* (feature-oci-via-podman
          #:key (podman-container-storage-driver 'btrfs)
          (storage-mount-point "/oci"))
  "Use Podman, binding its graph roots to STORAGE-MOUNT-POINT when that
filesystem is declared by feature-file-systems.  Hosts without it retain
their usual storage.  Existing stores must be migrated before enabling the
binding; the feature deliberately never copies or hides live data."

  (define (get-home-services _)
    (list
     (simple-service 'podman-packages home-profile-service-type
                     (list podman podman-compose tree-sitter-dockerfile))
     (service home-oci-service-type
              (for-home (oci-configuration
                         (runtime 'podman)
                         (verbose? #t))))))

  (define storage-driver
    (or (assoc-ref storage-drivers podman-container-storage-driver)
        (plain-file "storage.conf"
                    "[storage]")))

  (define (get-system-services config)
    (define subs
      ;; subgids/subuids changes only applyed after reboot!
      (list
       (subid-range (name (get-value 'user-name config)))
       (subid-range (name "oci-container"))
       (subid-range (name "cgroup"))
       ;; nobody is requrired for podman system migrate script that's sometime applied
       (subid-range (name "nobody"))))

    (append
     (if (and storage-mount-point
              (any (lambda (fs)
                     (and (file-system-mount? fs)
                          (string=? storage-mount-point
                                    (file-system-mount-point fs))))
                   (get-value 'file-systems config '())))
         (list (service podman-storage-service-type
                        `((directory . ,storage-mount-point)
                          (users . ("root" "oci-container"
                                    ,(get-value 'user-name config))))))
         '())
     (list
     (service iptables-service-type)
     (service oci-service-type
              (oci-configuration
               (runtime 'podman)
               (verbose? #t)))
     (service rootless-podman-service-type
              (rootless-podman-configuration
               (subgids subs)
               (subuids subs)
               (containers-storage storage-driver))))))

  (feature
   (name 'oci-via-podman)
   (values `((oci . #t)
             (oci-provider . podman)
             (podman . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
