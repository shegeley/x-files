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
  #:use-module ((gnu packages glib)       #:select (dbus))
  #:use-module ((gnu packages gnome)      #:select (gsettings-desktop-schemas
                                                    adwaita-icon-theme
                                                    mutter
                                                    gnome-shell
                                                    gnome-settings-daemon))
  #:use-module ((x-files packages phosh)  #:select (phosh phoc feedbackd-next))
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

(define* (phosh-session #:key (phosh phosh) (phoc phoc))
  "A launcher for the Phosh session, suitable as a greetd session command.
Sets the VM-safe wlroots environment, then starts phoc with Phosh as its
child (@code{phoc -E phosh}) inside a fresh D-Bus session, under a login shell
so the system profile's XDG_DATA_DIRS / GSettings schemas / PATH are in scope.

Note: this bypasses the packaged @code{phosh-session} wrapper, which drives
Phosh through @code{gnome-session}; that path lists @code{sm.puri.OSK0}
(squeekboard, the on-screen keyboard) as a *required* component and aborts the
session when it is absent.  Running phoc directly avoids the requirement, which
is fine in the VM (the host keyboard is used); packaging squeekboard and
restoring the gnome-session path is the follow-up for real hardware."
  (program-file
   "phosh-session-launcher"
   #~(begin
       (setenv "WLR_RENDERER" "pixman")
       (setenv "WLR_NO_HARDWARE_CURSORS" "1")
       (setenv "XDG_CURRENT_DESKTOP" "Phosh:GNOME")
       (execl #$(file-append bash "/bin/bash") "bash" "--login" "-c"
              (string-append
               "exec " #$(file-append dbus "/bin/dbus-run-session") " -- "
               #$(file-append phoc "/bin/phoc")
               " -C " #$(file-append phosh "/share/phosh/phoc.ini")
               " -E " #$(file-append phosh "/libexec/phosh"))))))

(define* (phosh-desktop-services
          #:key
          (autologin-user "phone")
          (phosh phosh)
          (phoc phoc))
  "Return the list of services that boot a system into the Phosh mobile shell:
greetd autologin into the Phosh session plus the logind/D-Bus/polkit/network
infrastructure it depends on, and the shell's runtime packages."
  (list
   (service greetd-service-type
            (greetd-configuration
             #:session-command (phosh-session #:phosh phosh #:phoc phoc)
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
   ;; Runtime bits Phosh expects on the system profile.  `dbus' is here for
   ;; `dbus-run-session', which phosh-session uses to spawn the compositor's
   ;; user D-Bus bus; gnome-session arrives via phosh's propagated inputs.
   (simple-service 'phosh-packages profile-service-type
                   (list phosh
                         phoc      ; installs the 'mobi.phosh.phoc' GSettings schema
                         mutter    ; 'org.gnome.mutter.keybindings' schema phoc reads
                         gnome-shell           ; 'org.gnome.shell.*' schemas phosh reads
                         gnome-settings-daemon ; 'org.gnome.settings-daemon.*' schemas
                         dbus
                         gsettings-desktop-schemas
                         adwaita-icon-theme
                         feedbackd-next
                         font-dejavu))))
