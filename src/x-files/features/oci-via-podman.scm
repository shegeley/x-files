(define-module (x-files features oci-via-podman)
  #:use-module (rde features)
  #:use-module (guix gexp)
  #:use-module ((gnu services containers) #:select (oci-service-type
                                                    oci-configuration
                                                    rootless-podman-service-type
                                                    rootless-podman-configuration))
  #:use-module ((gnu home services containers) #:select (home-oci-service-type))

  #:use-module ((gnu services) #:select (simple-service service for-home))

  #:use-module ((gnu services networking) #:select (iptables-service-type))
  #:use-module ((gnu system accounts) #:select (user-account
                                                user-group
                                                subid-range))

  #:use-module ((gnu system shadow) #:select (account-service-type))
  #:use-module ((gnu packages admin) #:select (shadow))

  #:export (feature-oci-via-podman))

(define storage-drivers
  `((btrfs . ,(plain-file "storage.conf"
                          "[storage]
driver = \"btrfs\"
"))))

(define* (feature-oci-via-podman
          #:key (podman-container-storage-driver 'btrfs))

  (define (get-home-services _)
    (list
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
               (containers-storage storage-driver)))))

  (feature
   (name 'oci-via-podman)
   (values `((oci . #t)
             (oci-provider . podman)
             (podman . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
