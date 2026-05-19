(define-module (x-files packages phosh)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages admin) #:select (libseat))
  #:use-module ((gnu packages freedesktop) #:select (appstream
                                                     elogind
                                                     libliftoff
                                                     libinput
                                                     libmbim
                                                     libqmi
                                                     modem-manager
                                                     wayland
                                                     wayland-protocols))
  #:use-module ((gnu packages fribidi) #:select (fribidi))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages gl) #:select (mesa))
  #:use-module ((gnu packages glib) #:select (dbus
                                              glib
                                              gobject-introspection))
  #:use-module ((gnu packages gnome) #:select (evince
                                               evolution-data-server
                                               feedbackd
                                               gcr-3
                                               gmobile
                                               gnome-bluetooth
                                               gnome-desktop
                                               gnome-session
                                               gsettings-desktop-schemas
                                               gsound
                                               json-glib
                                               libgudev
                                               libhandy
                                               libsecret
                                               libsoup
                                               network-manager
                                               upower
                                               vala))
  #:use-module ((gnu packages gtk) #:select (gtk+))
  #:use-module ((gnu packages linux) #:select (linux-pam))
  #:use-module ((gnu packages pciutils) #:select (hwdata))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages polkit) #:select (polkit))
  #:use-module ((gnu packages pulseaudio) #:select (pulseaudio))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages telephony) #:select (libcallaudio))
  #:use-module ((gnu packages wm) #:select (libdisplay-info wlroots))
  #:use-module ((gnu packages xorg) #:select (xcb-util-renderutil
                                               xcb-util-wm))
  #:use-module ((gnu packages xdisorg) #:select (libdrm
                                                  libxkbcommon
                                                  pixman))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; Polkit 124 — needed by phosh (requires polkit-agent-1 >= 0.122)
(define-public polkit-next
  (package
    (inherit polkit)
    (name "polkit-next")
    (version "124")
    (arguments
     (substitute-keyword-arguments (package-arguments polkit)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'patch-bash
              (lambda _
                (substitute* (list "subprojects/mocklibc-1.0/bin/mocklibc"
                                   "../source/test/data/etc/passwd"
                                   (string-append "../source/test/data/etc"
                                                  "/polkit-1/rules.d"
                                                  "/10-testing.rules"))
                  (("/bin/(bash|false|true)" _ command)
                   (which command)))))))))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/polkit/polkit")
                    (commit version)))
              (file-name (git-file-name "polkit" version))
              (sha256
               (base32
                "0lrs16d3zifnrvjjypfj4q0bz16ym5r0ddhmzcbmflxs2bdldksm"))
              ;; The polkit-disable-systemd.patch is already upstream in 124
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Look up actions and rules from /etc/polkit ...
                  (substitute* "src/polkitbackend/polkitbackendinteractiveauthority.c"
                    (("PACKAGE_DATA_DIR \"/polkit-1/actions\"")
                     "PACKAGE_SYSCONF_DIR \"/polkit-1/actions\""))
                  ;; ... but install package files below the prefix.
                  (substitute* "meson.build"
                    (("pk_sysconfdir = get_option\\('sysconfdir'\\)")
                     "pk_sysconfdir = get_option('prefix') / 'etc'")
                    ;; systemd_dep is undefined when using elogind
                    (("systemd_sysusers_dir = systemd_dep\\.get_pkgconfig_variable.*")
                     "systemd_sysusers_dir = '/usr/lib/sysusers.d'\n"))
                  ;; Set the setuid helper's real location.
                  (substitute* "src/polkitagent/polkitagentsession.c"
                    (("PACKAGE_PREFIX \"/lib/polkit-1/polkit-agent-helper-1\"")
                     "\"/run/setuid-programs/polkit-agent-helper-1\""))))))))

;; gmobile 0.7.1 — needed by phoc (requires >= 0.6.0)
(define-public gmobile-next
  (package
    (inherit gmobile)
    (name "gmobile-next")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/Phosh/gmobile")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "gmobile" version))
              (sha256
               (base32
                "1p257f4r7vg53d1p4m6nx3z2bvj8jda3l4f26nh01k0nizxhfya5"))))
    (native-inputs
     (list `(,glib "bin") gobject-introspection pkg-config vala))))

;; feedbackd 0.8.9 — needed by phosh (requires libfeedback >= 0.7.0)
(define-public feedbackd-next
  (package
    (inherit feedbackd)
    (name "feedbackd-next")
    (version "0.8.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/feedbackd/feedbackd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "feedbackd" version))
              (sha256
               (base32
                "0fygrlncz0h3m8h7r8w8i0ywd4y9cm5qlnsvzjn16bfv7kkcgip1"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (delete-file-recursively "subprojects")))))
    (inputs
     (list dbus gmobile-next gsound json-glib libgudev))))

(define-public libmbim-next
  (package
    (inherit libmbim)
    (name "libmbim-next")
    (version "1.32.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/mobile-broadband/libmbim")
                    (commit version)))
              (file-name (git-file-name "libmbim" version))
              (sha256
               (base32
                "1vgsx3smc0v8jbp15z07yv4z5vn9i6ry3b9fyjnb54gnw5g0v0pv"))))))

(define-public libqmi-next
  (package
    (inherit libqmi)
    (name "libqmi-next")
    (version "1.36.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/mobile-broadband/libqmi")
                    (commit version)))
              (file-name (git-file-name "libqmi" version))
              (sha256
               (base32
                "1ifk6q3v9fq3ms09ig8k9640djjf9flpz27plgypmz6f9g1nfqvh"))))
    (inputs
     (modify-inputs (package-inputs libqmi)
       (replace "libmbim" libmbim-next)))))

;; ModemManager 1.24.2 — needed by phosh (requires mm-glib >= 1.24)
(define-public modem-manager-next
  (package
    (inherit modem-manager)
    (name "modem-manager-next")
    (version "1.24.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/mobile-broadband/ModemManager")
                    (commit version)))
              (file-name (git-file-name "ModemManager" version))
              (sha256
               (base32
                "0aa2fgi4msgp0v5i2gjypfq1fcdc7s688vzwxd0n0qvvkjmcw4mc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments modem-manager)
       ((#:tests? _ #f) #f)))
    (inputs
     (modify-inputs (package-inputs modem-manager)
       (replace "libmbim" libmbim-next)
       (replace "libqmi" libqmi-next)))))

(define gvdb-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.gnome.org/GNOME/gvdb.git")
          (commit "4758f6fb7f889e074e13df3f914328f3eecb1fd3")))
    (file-name "gvdb-checkout")
    (sha256
     (base32 "129ln7rldafdkcf2kxdazckyp3xzxnk0xw33l2kzfc3bz4fahsp2"))))

(define wlroots-for-phoc-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.freedesktop.org/wlroots/wlroots.git")
          (commit "0.20.0")))
    (file-name "wlroots-for-phoc-checkout")
    (sha256
     (base32 "1gdmy4mpi7x23g81cdmcr9aakk3g6csl69h9j98yzssa48k6all5"))))

(define-public phoc
  (package
    (name "phoc")
    (version "0.55.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/Phosh/phoc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r5kafa17sgxr3dfmm40js2pi5b5rjqrrnvfzqsnlfv6hy824qp5"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dembed-wlroots=enabled"
              "-Dxwayland=disabled"
              "-Dtests=false"
              "-Dman=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-subprojects
            (lambda _
              (copy-recursively #+gvdb-source "subprojects/gvdb")
              (copy-recursively #+wlroots-for-phoc-source
                                "subprojects/wlroots-0.20.x")))
          (add-after 'copy-subprojects 'apply-wlroots-patches
            (lambda _
              (with-directory-excursion "subprojects/wlroots-0.20.x"
                (invoke "patch" "-p1" "-i"
                        "../../subprojects/packagefiles/wlroots/0001-Revert-layer-shell-error-on-0-dimension-without-anch.patch"))))
          (add-after 'copy-subprojects 'patch-paths
            (lambda _
              (substitute* "src/server.c"
                (("/bin/sh") (which "sh")))
              ;; Disable vulkan renderer to avoid glslang dependency
              (substitute* "meson.build"
                (("'renderers=gles2,vulkan'")
                 "'renderers=gles2'"))))
          (replace 'configure
            (lambda* (#:key outputs configure-flags #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (source-dir (getcwd)))
                (mkdir "../build")
                (chdir "../build")
                (apply invoke "meson" "setup"
                       (string-append "--prefix=" out)
                       "--buildtype=debugoptimized"
                       ;; Allow subprojects (no nofallback)
                       (string-append "-Dc_link_args=-Wl,-rpath=" out "/lib")
                       (string-append "-Dcpp_link_args=-Wl,-rpath=" out "/lib")
                       (append configure-flags
                               (list source-dir)))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           hwdata
           pkg-config
           python
           wayland-protocols))
    (inputs
     (list elogind
           gmobile-next
           gnome-desktop
           gsettings-desktop-schemas
           libdisplay-info
           libliftoff
           libdrm
           libinput
           libseat
           libxkbcommon
           mesa
           pixman
           wayland
           xcb-util-renderutil
           xcb-util-wm))
    (synopsis "Wayland compositor for mobile phones")
    (description
     "Phoc is a Wayland compositor for mobile phones based on wlroots.
It is the default compositor used by Phosh, the GNOME-based phone shell.")
    (home-page "https://gitlab.gnome.org/World/Phosh/phoc")
    (license license:gpl3+)))

(define gvc-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.gnome.org/GNOME/libgnome-volume-control.git")
          (commit "d2442f455844e5292cb4a74ffc66ecc8d7595a9f")))
    (file-name "gvc-checkout")
    (sha256
     (base32 "16rpb6iygdlwrkknp35k8lls66hv655rhsqp86pzk6zvavizhjfp"))))

(define libcall-ui-for-phosh-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.gnome.org/World/Phosh/libcall-ui")
          (commit "v0.1.5")))
    (file-name "libcall-ui-for-phosh-checkout")
    (sha256
     (base32 "0zj6jrm21djsqn56rydryxys10wqxp6y4zskwx52nxjr4k0r6m72"))))

(define-public phosh
  (package
    (name "phosh")
    (version "0.55.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/Phosh/phosh")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pmfsgw4rm6dznfqlyj5fqfwy91h9ynf6nyisyc6x9kk1by6sapy"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-Dtests=false"
              "-Dman=false"
              "-Dlockscreen-plugins=false"
              "-Dquick-setting-plugins=false"
              "-Dstatus-icon-plugins=false"
              (string-append "-Dcompositor="
                             (search-input-file %build-inputs "/bin/phoc")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-subprojects
            (lambda _
              (copy-recursively #+gvc-source "subprojects/gvc")
              (copy-recursively #+libcall-ui-for-phosh-source
                                "subprojects/libcall-ui")))
          (add-after 'copy-subprojects 'patch-phoc-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "data/phosh-session.in"
                (("@bindir@/phoc")
                 (search-input-file inputs "/bin/phoc"))))))))
    (native-inputs
     (list gcr-3
           gettext-minimal
           `(,glib "bin")
           `(,gtk+ "bin")
           gobject-introspection
           pkg-config
           wayland-protocols))
    (inputs
     (list appstream
           elogind
           evince
           evolution-data-server
           feedbackd-next
           fribidi
           gmobile-next
           gnome-bluetooth
           gnome-desktop
           gsettings-desktop-schemas
           gtk+
           libgudev
           libhandy
           libcallaudio
           libsecret
           libsoup
           linux-pam
           modem-manager-next
           network-manager
           phoc
           polkit-next
           pulseaudio
           upower
           wayland))
    (propagated-inputs
     (list gsettings-desktop-schemas
           gnome-session))
    (synopsis "Wayland shell for mobile devices")
    (description
     "Phosh is a graphical shell for mobile devices based on GNOME technologies.
It provides a touch-friendly interface with support for phone calls,
notifications, and adaptive applications.")
    (home-page "https://gitlab.gnome.org/World/Phosh/phosh")
    (license license:gpl3+)))
