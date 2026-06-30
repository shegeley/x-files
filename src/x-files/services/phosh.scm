(define-module (x-files services phosh)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module ((gnu services desktop)    #:select (elogind-service-type
                                                    upower-service-type))
  #:use-module ((gnu services dbus)       #:select (dbus-root-service-type
                                                    polkit-service-type))
  #:use-module ((gnu services networking) #:select (network-manager-service-type
                                                    modem-manager-service-type
                                                    wpa-supplicant-service-type))
  #:use-module ((gnu packages bash)       #:select (bash))
  #:use-module ((gnu packages fonts)      #:select (font-dejavu))
  #:use-module ((gnu packages gnome)      #:select (gsettings-desktop-schemas
                                                    adwaita-icon-theme))
  #:use-module ((x-files packages phosh)  #:select (phosh feedbackd-next))
  #:use-module ((x-files services greetd) #:select (greetd-service-type
                                                    greetd-configuration))
  #:export (phosh-session
            phosh-desktop-services))

;;;
;;; Phosh mobile session, and the set of services needed to boot into it.
;;;
;;; The Phosh shell runs on top of the `phoc' Wayland compositor (wlroots).  In
;;; a QEMU VM there is no GPU, so we force wlroots' software (pixman) renderer
;;; and disable hardware cursors; these env vars are the difference between a
;;; black screen and a working shell under emulation.
;;;
;;; `phosh-desktop-services' is the phone analogue of %desktop-services: it wires
;;; greetd (autologin) to the Phosh session and adds the logind/dbus/polkit/
;;; network stack a Wayland shell needs, reusing the upstream Guix services.
;;;

(define* (phosh-session #:key (phosh phosh))
  "A launcher for the Phosh session, suitable as a greetd session command.
Sets the VM-safe wlroots environment, then execs the package's phosh-session
wrapper (which starts phoc and the shell) under a login shell so the system
profile's XDG_DATA_DIRS / GSettings schemas / PATH are in scope."
  (program-file
   "phosh-session-launcher"
   #~(begin
       (setenv "WLR_RENDERER" "pixman")
       (setenv "WLR_NO_HARDWARE_CURSORS" "1")
       (setenv "XDG_CURRENT_DESKTOP" "Phosh:GNOME")
       (execl #$(file-append bash "/bin/bash") "bash" "--login" "-c"
              (string-append
               "exec " #$(file-append phosh "/bin/phosh-session"))))))

(define* (phosh-desktop-services
          #:key
          (autologin-user "phone")
          (phosh phosh))
  "Return the list of services that boot a system into the Phosh mobile shell:
greetd autologin into the Phosh session plus the logind/D-Bus/polkit/network
infrastructure it depends on, and the shell's runtime packages."
  (list
   (service greetd-service-type
            (greetd-configuration
             #:session-command (phosh-session #:phosh phosh)
             #:autologin-user  autologin-user))
   ;; Seat/session management (logind), the system bus, and authorization.
   (service elogind-service-type)
   (service dbus-root-service-type)
   (service polkit-service-type)
   ;; Networking + modem (cellular is inert in the VM but kept for realism).
   ;; wpa-supplicant provides the 'wireless-daemon NetworkManager requires.
   (service wpa-supplicant-service-type)
   (service network-manager-service-type)
   (service modem-manager-service-type)
   (service upower-service-type)
   ;; Runtime bits Phosh expects on the system profile.
   (simple-service 'phosh-packages profile-service-type
                   (list phosh
                         gsettings-desktop-schemas
                         adwaita-icon-theme
                         feedbackd-next
                         font-dejavu))))
