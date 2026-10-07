(define-module (x-files packages penguin-mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (glib-networking
                                               libadwaita
                                               librsvg))
  #:use-module ((gnu packages gtk) #:select (cairo
                                             gdk-pixbuf
                                             graphene
                                             gtk
                                             pango))
  #:use-module ((gnu packages webkit) #:select (webkitgtk))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary))

(define-public penguin-mail
  (package
    (name "penguin-mail")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/c9dev/penguin-mail/releases/download/v"
             version "/penguin-mail-" version "-x86_64.tar.gz"))
       (sha256
        (base32 "1mq2bfgqn8sdnv232dkxnn81129f5sriash5b61k1lmgx2r8nzzy"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:patchelf-plan
      `'(("bin/penguin-mail"
          ("libc"
           "gcc"
           "glib"
           "cairo"
           "pango"
           "graphene"
           "gdk-pixbuf"
           "gtk"
           "libadwaita"
           "webkitgtk"))
        ("bin/penguin-mail-cli"
         ("libc"
          "gcc")))
      #:install-plan
      `'(("bin/" "bin")
         ("share/" "share"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-gio-modules
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; TLS for GIO/libsoup (WebKit remote content, OAuth pages)
              ;; lives in glib-networking's GIO module, found via this
              ;; variable rather than a link-time dependency.
              (wrap-program (search-input-file outputs "bin/penguin-mail")
                `("GIO_EXTRA_MODULES" prefix
                  (,(dirname (search-input-file
                              inputs "lib/gio/modules/libgiognutls.so"))))))))))
    (inputs
     (list `(,gcc "lib")
           glib
           cairo
           pango
           graphene
           gdk-pixbuf
           gtk
           libadwaita
           glib-networking
           librsvg                    ;SVG app icon in the hicolor theme
           webkitgtk))
    ;; gpg/gpgsm (OpenPGP, S/MIME) and hunspell dictionaries are looked up on
    ;; PATH at run time, deliberately not baked in: they belong to the user's
    ;; own GnuPG/desktop setup.
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/c9dev/penguin-mail")
    (synopsis "Mail and calendar for Linux (Gmail, Microsoft, IMAP), GTK4/libadwaita")
    (description
     "Penguin Mail is a mail and calendar client for Linux.  It reads Gmail,
Microsoft (Outlook.com, Hotmail, Live, Microsoft 365) and any IMAP/SMTP
account, syncs from the system tray, and keeps mail on the local computer.
It offers conversations, Gmail search and categories, OpenPGP and S/MIME
through the system's own GnuPG, rules, a calendar, contacts, and an optional
assistant.  It is written in Rust with GTK 4 and libadwaita.

This package repacks the official upstream binary release; the Google and
Microsoft OAuth clients compiled into it make account sign-in work out of
the box.  @code{penguin-mail --demo} opens the app on sample accounts.")
    (license license:gpl3+)))
