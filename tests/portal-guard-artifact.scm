(use-modules (guix gexp)
             ((guix modules) #:select (source-module-closure))
             ((gnu services) #:select (service-value))
             ((gnu packages guile) #:select (guile-3.0))
             ((gnu packages glib) #:select (dbus glib))
             ((gnu packages freedesktop) #:select (elogind))
             ((rde features) #:select (feature-home-services-getter))
             ((srfi srfi-1) #:select (append-map filter find second))
             ((x-files features desktop) #:select (feature-desktop-services)))

(define (inputs expression)
  "Return the file-like inputs referenced by @var{expression}."
  (append-map
   (lambda (reference)
     (let ((input (gexp-input-thing reference)))
       (if (list? input) input (list input))))
   (filter gexp-input? ((@@ (guix gexp) gexp-references) expression))))

(define (portal-guard-test-artifacts)
  "Return buildable guard, activation, and D-Bus test programs.
Extract the actual @code{feature-desktop-services} autostart command and
activation gexp without activating a home generation."
  (let* ((services ((feature-home-services-getter (feature-desktop-services)) #f))
         (activation (service-value (second services)))
         (desktop (find computed-file? (inputs activation)))
         (guard (find program-file? (inputs (computed-file-gexp desktop)))))
    (file-union "portal-guard-test-artifacts"
                `(("guard" ,guard)
                  ("guile" ,(file-append guile-3.0 "/bin/guile"))
                  ("busctl" ,(file-append elogind "/bin/busctl"))
                  ("dbus-run-session" ,(file-append dbus "/bin/dbus-run-session"))
                  ("libgio.so" ,(file-append glib "/lib/libgio-2.0.so"))
                  ("activate" ,(program-file
                                "activate-portal-guard"
                                (with-imported-modules
                                    (source-module-closure '((guix build utils)))
                                  #~#$activation)))
                  ("autostart.desktop" ,desktop)))))

(portal-guard-test-artifacts)
