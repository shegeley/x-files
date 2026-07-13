(define-module (x-files packages ayatana)
  #:use-module ((guix licenses)             #:prefix license:)
  #:use-module ((guix packages)             #:select (package origin base32))
  #:use-module ((guix git-download)         #:select (git-fetch
                                                       git-reference
                                                       git-file-name))
  #:use-module ((guix build-system cmake)   #:select (cmake-build-system))
  #:use-module ((gnu packages glib)         #:select (glib
                                                       gobject-introspection))
  #:use-module ((gnu packages gtk)          #:select (gtk+ libdbusmenu))
  #:use-module ((gnu packages gnome)        #:select (vala))
  #:use-module ((gnu packages pkg-config)   #:select (pkg-config))
  #:use-module (guix gexp))

;; The Ayatana Indicators stack (a maintained fork of Canonical's old
;; libindicator/libappindicator).  Packaged here solely because the prebuilt
;; Spotify snap hard-links these three libraries; Guix proper ships none of
;; them.  Recent releases vendor their own CMake helper modules, so no
;; cmake-extras dependency is required.

(define-public ayatana-ido
  (package
    (name "ayatana-ido")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AyatanaIndicators/ayatana-ido")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pi0qan239chglhi1vx8ndlx3ppr3xjrr7alan0vw6m06wx89kmd"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; The test suite pulls in gtest and needs an X display; the library
      ;; itself has no runtime need for it.
      #:tests? #f
      #:configure-flags #~(list "-DENABLE_TESTS=OFF")))
    (native-inputs
     (list pkg-config
           `(,glib "bin")               ;glib-mkenums / glib-genmarshal
           gobject-introspection
           vala))                       ;.vapi generation in the GIR block
    (inputs (list glib gtk+))
    (home-page "https://github.com/AyatanaIndicators/ayatana-ido")
    (synopsis "Ayatana Indicator Display Objects library")
    (description
     "Ayatana-IDO provides custom GTK widgets used by the Ayatana Indicators
menus (calendar, scale, entry, etc.).  It installs
@code{libayatana-ido3-0.4.so}.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public libayatana-indicator
  (package
    (name "libayatana-indicator")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AyatanaIndicators/libayatana-indicator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08n4fjnc1cbjynqb3r6vik994yl6wbgi7q6p0h19xwrxx94lixym"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      ;; ENABLE_LOADER builds an ayatana-indicator-loader3 helper that links
      ;; the just-built libayatana-indicator3.so.7 but gets no $out/lib in its
      ;; RUNPATH (failing validate-runpath).  Nothing we ship uses that tool,
      ;; so drop it rather than patch its RUNPATH.
      #:configure-flags
      #~(list "-DENABLE_TESTS=OFF"
              "-DENABLE_LOADER=OFF")))
    (native-inputs
     (list pkg-config
           `(,glib "bin")))
    (inputs (list glib))
    ;; ayatana-indicator3-0.4.pc has "Requires: gtk+-3.0 libayatana-ido3-0.4",
    ;; so both must propagate for downstream pkg-config to resolve.
    (propagated-inputs (list gtk+ ayatana-ido))
    (home-page "https://github.com/AyatanaIndicators/libayatana-indicator")
    (synopsis "Ayatana Indicators shared library")
    (description
     "Libayatana-indicator is the shared support library for the Ayatana
Indicators: it loads indicator modules and renders their menus.  It installs
@code{libayatana-indicator3.so.7}.")
    (license license:gpl3)))

(define-public libayatana-appindicator
  (package
    (name "libayatana-appindicator")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AyatanaIndicators/libayatana-appindicator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j28gl3hkl3iparp2dai819cspgqx6apshb7w897kmbwnyzy7akh"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      ;; Vala/Mono language bindings and gtk-doc are irrelevant for the plain
      ;; C library that consumers (e.g. Spotify) dlopen.
      #:configure-flags
      #~(list "-DENABLE_TESTS=OFF"
              "-DENABLE_GTKDOC=OFF"
              "-DENABLE_BINDINGS_VALA=OFF"
              "-DENABLE_BINDINGS_MONO=OFF")))
    (native-inputs
     (list pkg-config
           `(,glib "bin")
           gobject-introspection))
    (inputs (list glib))
    ;; ayatana-appindicator3-0.1.pc has "Requires: ayatana-indicator3-0.4
    ;; gtk+-3.0 dbusmenu-gtk3-0.4"; propagate them for downstream pkg-config
    ;; and to satisfy the GObject-introspection scanner at build time.
    (propagated-inputs (list gtk+ libayatana-indicator libdbusmenu))
    (home-page "https://github.com/AyatanaIndicators/libayatana-appindicator")
    (synopsis "Ayatana application indicators (system tray) library")
    (description
     "Libayatana-appindicator lets applications export a menu into an Ayatana
Indicator system-tray area over D-Bus, falling back to a GtkStatusIcon where
no host is present.  It installs @code{libayatana-appindicator3.so.1}.")
    (license (list license:lgpl2.1 license:lgpl3 license:gpl3))))
