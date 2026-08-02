(define-module (x-files features gnome-desktop)
  #:use-module (rde features)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages)
  #:use-module ((gnu packages networking) #:select (blueman))
  #:use-module ((guix gexp) #:select (plain-file))
  #:use-module ((gnu home services) #:select (home-xdg-configuration-files-service-type))
  #:use-module (gnu home services xdg)

  #:use-module (x-files packages gpaste)

  #:export (feature-gnome-desktop
            themes
            packages))

(define themes
  (map
   specification->package
   (list
    "arc-theme"
    "arc-icon-theme"
    "materia-theme"
    "nordic-theme"
    "numix-gtk-theme"
    "delft-icon-theme"
    "flat-remix-icon-theme"
    "flat-remix-gnome-theme"
    "flat-remix-gtk-theme"
    "orchis-theme")))

(define packages
  (append
   (list gpaste)
   (map
    specification->package
    (list
     "nautilus"
     "evince"
     ;; "glib" ;; gsettings
     "seahorse" ;; Gnome GUI key management
     "shotwell"
     "glade"
     "xdg-utils"
     "xdg-dbus-proxy"
     "xdg-desktop-portal-gnome" ;; needed for screenshare
     "system-config-printer"
     "dconf"
     "dconf-editor"
     "gnome-terminal"
     "gnome-bluetooth"
     "bluez"
     "gnome-tweaks"
     "gnome-shell-extensions"
     "blueman"))))

(define extensions
  (map specification->package (list "gnome-shell-extension-blur-my-shell")))

(define mimes
 `((inode/directory . nautilus.desktop)
   (image/jpeg . org.gnome.Shotwell-Viewer.desktop)
   (image/jpg . org.gnome.Shotwell-Viewer.desktop)
   (image/png . org.gnome.Shotwell-Viewer.desktop)
   (image/gif . org.gnome.Shotwell-Viewer.desktop)
   (image/wepb . org.gnome.Shotwell-Viewer.desktop)
   (application/djvu . evince.desktop)
   (application/pdf . evince.desktop)))

;; On GNOME, ScreenCast/RemoteDesktop must be handled by the gnome portal backend
;; (it drives Mutter's screencast).  The sway feature also installs
;; xdg-desktop-portal-wlr; without pinning the interface, browsers can end up
;; with no working screen source ("share screen" offers nothing).  Named
;; `gnome-portals.conf' so xdg-desktop-portal only applies it under GNOME — sway
;; keeps routing to wlr.
(define %gnome-portals.conf
  (plain-file "gnome-portals.conf"
    (string-append
     "[preferred]\n"
     "default=gnome;gtk\n"
     "org.freedesktop.impl.portal.ScreenCast=gnome\n"
     "org.freedesktop.impl.portal.RemoteDesktop=gnome\n")))

(define* (feature-gnome-desktop
          #:key
          (libnotify (specification->package "libnotify"))
          (packages packages)
          (themes themes)
          (mimes mimes)
          (configuration (gnome-desktop-configuration)))

  "RDE-compliant"

  (define (get-home-services config)
    (list
     (simple-service
       'gnome-apps-mime-entries
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration (default mimes)))
     (simple-service
       'gnome-screencast-portal
       home-xdg-configuration-files-service-type
       (list (list "xdg-desktop-portal/gnome-portals.conf" %gnome-portals.conf)))))

  (define (get-system-services config)
    (list
     (simple-service 'blueman dbus-root-service-type (list blueman))
     (service gnome-desktop-service-type configuration)
     (simple-service 'add-gnome-extensions profile-service-type extensions)
     (simple-service 'add-gnome-themes profile-service-type themes)
     (simple-service 'add-gnome-packages profile-service-type (cons libnotify packages))))
  (feature
   (name 'gnome-desktop)
   (values `((gnome-desktop . #t)
             (wayland . #t)
             ;; have to conform with swaynotification center from rde
             (desktop-notifications . #t)
             (libnotify . ,libnotify)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
