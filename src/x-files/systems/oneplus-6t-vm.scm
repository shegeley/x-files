(define-module (x-files systems oneplus-6t-vm)
  #:use-module ((gnu system)            #:select (operating-system
                                                  %base-packages))
  #:use-module ((gnu system file-systems) #:select (file-system
                                                    file-system-label
                                                    %base-file-systems))
  #:use-module ((gnu system shadow)     #:select (user-account
                                                  user-group
                                                  %base-user-accounts))
  #:use-module ((gnu bootloader)        #:select (bootloader-configuration))
  #:use-module ((gnu bootloader grub)   #:select (grub-efi-bootloader))
  #:use-module ((gnu services)          #:select (service))
  #:use-module ((gnu services base)     #:select (%base-services))
  #:use-module ((gnu services ssh)      #:select (openssh-service-type
                                                  openssh-configuration))
  #:use-module ((gnu packages gl)       #:select (mesa))
  #:use-module ((gnu packages terminals) #:select (foot))
  #:use-module ((x-files services phosh) #:select (phosh-desktop-services))
  #:export (make-phone-os
            oneplus-6t-vm-os))

;;;
;;; A QEMU/UEFI virtual machine that boots straight into the Phosh mobile shell.
;;;
;;; This is the "try it out" stand-in for the OnePlus 6T (fajita): a VM cannot
;;; emulate the Snapdragon 845 hardware, but it exercises the whole transferable
;;; software stack -- greetd autologin, Phosh/phoc, logind, the mobile services.
;;;
;;; The operating-system itself is architecture-agnostic; pick the architecture
;;; at build time:
;;;
;;;   aarch64 (matches the device's CPU, emulated/slow):
;;;     guix system image -L src --image-type=efi-raw --system=aarch64-linux \
;;;       -e '(@ (x-files systems oneplus-6t-vm) oneplus-6t-vm-os)'
;;;
;;;   x86_64 (fast KVM sanity check of the same config):
;;;     guix system image -L src --image-type=efi-raw --system=x86_64-linux \
;;;       -e '(@ (x-files systems oneplus-6t-vm) oneplus-6t-vm-os)'
;;;
;;; It is built with the `efi-raw' image type, whose GPT layout is an ESP
;;; labelled "GNU-ESP" plus an ext4 root labelled "Guix_image"; the file-systems
;;; below mount those by label.
;;;

(define* (make-phone-os
          #:key
          (host-name "fajita-vm")
          (user      "phone")
          (timezone  "Etc/UTC"))
  (operating-system
    (host-name host-name)
    (timezone timezone)
    (locale "en_US.utf8")
    ;; Keyboard layout is left at the default (US QWERTY), which suits the VM.

    ;; grub-efi: the efi-raw image installs GRUB onto the ESP for us; this also
    ;; provides the running system's grub.cfg.  Works for aarch64 and x86_64.
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets '("/boot/efi"))))

    (file-systems
     (cons* (file-system
              (mount-point "/")
              (device (file-system-label "Guix_image"))
              (type "ext4"))
            (file-system
              (mount-point "/boot/efi")
              (device (file-system-label "GNU-ESP"))
              (type "vfat"))
            %base-file-systems))

    (users
     (cons (user-account
            (name user)
            ;; Throwaway VM credentials; greetd autologins this user, and the
            ;; password ("1234") also lets you log in over SSH for debugging.
            (password (crypt "1234" "$6$fajita"))
            (group "users")
            (supplementary-groups
             '("wheel" "netdev" "audio" "video" "input" "tty"))
            (home-directory (string-append "/home/" user))
            (comment "Phosh VM user"))
           %base-user-accounts))

    (packages
     (cons* mesa foot %base-packages))

    (services
     (append
      (phosh-desktop-services #:autologin-user user)
      (list
       ;; SSH in for debugging (port-forwarded by the QEMU run target).
       (service openssh-service-type
                (openssh-configuration
                 (permit-root-login #f)
                 (password-authentication? #t))))
      %base-services))))

(define oneplus-6t-vm-os (make-phone-os))
