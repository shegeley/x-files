(define-module (x-files features desktop)
  #:use-module ((rde features) #:select (feature))
  #:use-module ((gnu services) #:select (service simple-service))
  #:use-module (guix gexp)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module ((srfi srfi-1) #:select (first))

  #:use-module ((gnu services avahi) #:select (avahi-service-type
                                               avahi-configuration))

  #:use-module ((gnu services desktop) #:select (elogind-service-type
                                                 elogind-configuration
                                                 udisks-service-type
                                                 udisks-configuration
                                                 geoclue-service-type
                                                 geoclue-configuration
                                                 upower-service-type
                                                 upower-configuration))

  #:use-module ((gnu services dbus) #:select (dbus-root-service-type
                                              dbus-configuration))

  #:use-module ((gnu home services desktop) #:select (home-dbus-service-type
                                                      home-dbus-configuration))

  #:use-module ((gnu home services) #:select (home-activation-service-type))

  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module ((gnu packages dns) #:select (avahi))
  #:use-module ((gnu packages freedesktop) #:select (elogind
                                                     udisks))
  #:use-module ((gnu packages gnome) #:select (geoclue
                                               upower))

  #:export (feature-desktop-services))

(define %rde-desktop-system-services
  (@@ (rde features base) %rde-desktop-system-services))

(define* (feature-desktop-services
          #:key
          (default-desktop-system-services %rde-desktop-system-services)
          (avahi avahi)
          (dbus dbus)
          (elogind elogind)
          (geoclue geoclue)
          (udisks udisks)
          (upower upower))
  "Return configurable RDE desktop services and a GNOME portal guard.

Extend @var{default-desktop-system-services} with the supplied desktop
packages, including an elogind configuration that ignores lid switches.
The home services provide D-Bus and install the guard in
@file{~/.config/autostart/gnome-portal-backend-guard.desktop}.

Home activation atomically replaces the autostart entry, including a
read-only entry from an earlier generation.  Repeated activation and
rollback use the same operation; installation failures are caught and logged.
Activation does not start or stop portals.

At GNOME login, wait for the shell and probe the backend before the public
portal.  An unhealthy owner receives at most one @code{SIGTERM}; subsequent
D-Bus probes activate its replacement.  The guard works with the stock
GNOME backend.  Run its autostart @code{Exec} command with @option{--check}
to inspect source masks without restarting an existing owner."

  (define gnome-portal-guard
    (program-file
     "gnome-portal-backend-guard"
     (with-imported-modules
         (source-module-closure
          '((x-files features desktop portal-guard))
          #:select? (lambda (name) (eq? (first name) 'x-files)))
       #~(begin
           (use-modules ((x-files features desktop portal-guard)
                         #:select (repair-screen-cast-portal!))
                        ((ice-9 match) #:select (match))
                        ((ice-9 popen) #:select (close-pipe
                                                open-pipe*))
                        ((ice-9 ports) #:select (OPEN_READ))
                        ((ice-9 rdelim) #:select (read-line))
                        ((srfi srfi-13) #:select (string-contains-ci
                                                 string-tokenize)))

           (define busctl #$(file-append elogind "/bin/busctl"))
           (define kill #$(file-append coreutils "/bin/kill"))
           (define portal-object "/org/freedesktop/portal/desktop")
           (define shell-service "org.gnome.Shell")
           (define backend-service
             "org.freedesktop.impl.portal.desktop.gnome")
           (define portal-service "org.freedesktop.portal.Desktop")
           (define backend-interface
             "org.freedesktop.impl.portal.ScreenCast")
           (define portal-interface "org.freedesktop.portal.ScreenCast")

           (define (run-line . arguments)
             (let* ((port (apply open-pipe* OPEN_READ arguments))
                    (line (read-line port))
                    (status (close-pipe port)))
               (and (zero? status)
                    (string? line)
                    line)))

           (define (run-uint32 . arguments)
             (let ((line (apply run-line arguments)))
               (and line
                    (match (string-tokenize line)
                      (("u" value) (string->number value))
                      (_ #f)))))

           (define (owner-pid service)
             (run-uint32
              busctl "--user" "--timeout=5s" "call"
              "org.freedesktop.DBus"
              "/org/freedesktop/DBus"
              "org.freedesktop.DBus"
              "GetConnectionUnixProcessID"
              "s" service))

           (define (source-types service interface)
             (run-uint32
              busctl "--user" "--timeout=5s" "get-property"
              service portal-object interface "AvailableSourceTypes"))

           (define (wait-for-owner-change service old-pid)
             (let loop ((remaining 10))
               (let ((pid (owner-pid service)))
                 (cond
                  ((or (not pid) (not (= pid old-pid))) #t)
                  ((<= remaining 1) #f)
                  (else
                   (sleep 1)
                   (loop (1- remaining)))))))

           (define (restart-owner! service)
             (let ((pid (owner-pid service)))
               (or (not pid)
                   (and (zero? (system* kill "-TERM"
                                       (number->string pid)))
                        (wait-for-owner-change service pid)))))

           (define (gnome-session?)
             (let ((desktop (getenv "XDG_CURRENT_DESKTOP")))
               (and desktop (string-contains-ci desktop "GNOME"))))

           (define (check-result)
             `((status . check)
               (backend-source-types
                . ,(source-types backend-service backend-interface))
               (portal-source-types
                . ,(source-types portal-service portal-interface))
               (actions . ())))

           (define (repair-result)
             (repair-screen-cast-portal!
              #:shell-ready? (lambda () (owner-pid shell-service))
              #:backend-source-types
              (lambda () (source-types backend-service backend-interface))
              #:portal-source-types
              (lambda () (source-types portal-service portal-interface))
              #:restart-backend! (lambda () (restart-owner! backend-service))
              #:restart-portal! (lambda () (restart-owner! portal-service))))

           (define (positive-source-types? value)
             (and (integer? value) (positive? value)))

           (define (successful? result)
             (memq (assoc-ref result 'status) '(healthy repaired)))

           (define (check-successful? result)
             (and (positive-source-types?
                   (assoc-ref result 'backend-source-types))
                  (positive-source-types?
                   (assoc-ref result 'portal-source-types))))

           (exit
            (catch #t
              (lambda ()
                (if (not (gnome-session?))
                    0
                    (let* ((check-only?
                            (member "--check" (command-line)))
                           (result
                            (if check-only?
                                (check-result)
                                (repair-result))))
                      (format #t "gnome-portal-backend-guard: ~s~%" result)
                      (if ((if check-only?
                               check-successful?
                               successful?)
                           result)
                          0
                          1))))
              (lambda (key . arguments)
                (format (current-error-port)
                        "gnome-portal-backend-guard: ~s ~s~%"
                        key arguments)
                1)))))))

  (define portal-backend-guard-desktop
    (mixed-text-file
     "gnome-portal-backend-guard.desktop"
     "[Desktop Entry]\n"
     "Type=Application\n"
     "Name=GNOME ScreenCast portal guard\n"
     "Comment=Repair stale ScreenCast backend and frontend portal state after GNOME Shell starts\n"
     "Exec=" gnome-portal-guard "\n"
     "Terminal=false\n"
     "NoDisplay=true\n"
     "X-GNOME-Autostart-enabled=true\n"))

  (define (get-home-services _)
    (list (service home-dbus-service-type
                   (home-dbus-configuration (dbus dbus)))
          (simple-service 'gnome-portal-backend-guard-autostart
                          home-activation-service-type
                          #~(begin
                              (use-modules
                               ((guix build utils) #:select (mkdir-p)))
                              (catch #t
                                (lambda ()
                                  (let ((dir
                                         (string-append
                                          (getenv "HOME")
                                          "/.config/autostart")))
                                    (mkdir-p dir)
                                    (let* ((temporary
                                            (string-append dir "/.portal-guard.XXXXXX"))
                                           (port (mkstemp! temporary)))
                                      (close-port port)
                                      (dynamic-wind
                                        (lambda () #t)
                                        (lambda ()
                                          (copy-file #$portal-backend-guard-desktop
                                                     temporary)
                                          (rename-file
                                           temporary
                                           (string-append
                                            dir "/gnome-portal-backend-guard.desktop")))
                                        (lambda ()
                                          (when (file-exists? temporary)
                                            (delete-file temporary)))))))
                                (lambda (key . arguments)
                                  (format
                                   (current-error-port)
                                   "gnome-portal guard activation: ~s ~s~%"
                                   key arguments)
                                  #f))))))

  (define (get-system-services _)
    (cons*
     (service avahi-service-type
              (avahi-configuration (avahi avahi)))
     (service dbus-root-service-type
              (dbus-configuration (dbus dbus)))
     (service elogind-service-type
              (elogind-configuration
                (elogind elogind)
                (handle-lid-switch 'ignore)
                (handle-lid-switch-external-power 'ignore)
                (handle-lid-switch-docked 'ignore)))
     (service geoclue-service-type
              (geoclue-configuration (geoclue geoclue)))
     (service udisks-service-type
              (udisks-configuration (udisks udisks)))
     (service upower-service-type
              (upower-configuration (upower upower)))
     default-desktop-system-services))

  (feature
   (name 'desktop-services)
   (values `((desktop-services . #t)
             (elogind . ,elogind)
             (dbus . ,dbus)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
