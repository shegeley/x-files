(define-module (x-files packages max)
  #:use-module ((nonguix licenses)             #:select (nonfree))
  #:use-module ((guix packages)                #:select (package
                                                         origin
                                                         base32
                                                         this-package-input))
  #:use-module ((guix download)                #:select (url-fetch))
  #:use-module ((gnu packages)                 #:select (specification->package))
  #:use-module ((gnu packages guile-xyz)       #:select (guile-ini
                                                        guile-smc
                                                        guile-lib))
  #:use-module ((nonguix build-system binary)  #:select (binary-build-system))
  #:use-module (guix gexp))

;; guile-ini's (ini) module pulls in (ini fsm), which needs guile-smc and
;; guile-lib; all three must be on the builder's load path -- same set as
;; (x-files packages slojka)/kadr.
(define %max-guile-ini-extensions (list guile-ini guile-smc guile-lib))

;; MAX (max.ru) is a Russian messenger.  It ships as a prebuilt Qt6/QML
;; desktop client (not Electron -- Categories=Qt in its own .desktop, and a
;; qt.conf sibling to every executable), bundling its own private Qt6 build,
;; ffmpeg, and OpenSSL under usr/share/max/{bin,lib,lib64,libexec,plugins,qml}.
;; Only via the official apt repo (download.max.ru/linux-repos is a
;; client-rendered landing page that 404-SPA-falls-back on every path; the
;; real repo lives one level down at download.max.ru/linux/deb, an aptly-
;; generated repo with no separate detached signature seen on the wire --
;; nothing here to verify against, hence plain url-fetch straight to the
;; pool .deb, same trust model as (x-files packages yaak)).
;;
;; nonguix's binary-build-system natively unpacks .deb sources (`ar x' +
;; `tar xf data.tar.*'), so, like (x-files packages yaak), no custom unpack
;; phase is needed.
;;
;; The bundled usr/share/max/bin/{max,max-service/bin/max-service} and
;; usr/share/max/libexec/QtWebEngineProcess already carry a working,
;; relative ($ORIGIN-based) RPATH to their own sibling lib64 -- confirmed via
;; `ldd': every bundled Qt6/ffmpeg/private .so resolves without
;; LD_LIBRARY_PATH.  What does NOT resolve is a real, if partial, set of
;; system libraries these binaries need directly (X11/xcb, mesa/libglvnd's
;; GL/GLX/EGL/gbm, glib, xkbcommon, nss/nspr, dbus, alsa, pulseaudio,
;; va/vdpau, a GTK3 platform-theme plugin, ...) that this .deb declines to
;; bundle, relying instead on the target distro to have them installed
;; (matching its Debian control file's Depends/Recommends, expanded here
;; with whatever `ldd'/exhaustively scanning plugins/ and qml/ turned up
;; still missing).
;;
;; RPATHs are set in a custom phase invoking `patchelf' directly (see
;; patch-max-binaries below) rather than through binary-build-system's own
;; #:patchelf-plan: that field only accepts a plain (non-gexp) Scheme
;; value, and gexp's embedding of a plain value into the generated builder
;; script inserts it bare/unprotected -- landing in an evaluated
;; (function-argument) position, so the raw nested list gets evaluated as
;; code instead of read as data.  A custom phase sidesteps the field
;; entirely and reuses the same #$(this-package-input ...)/file-append
;; gexp-splicing already used a few lines down for the launcher's
;; LD_LIBRARY_PATH.
;;
;; The huge surface of Qt plugin .so's under plugins/ and qml/ is
;; deliberately left unpatched (patching each individually is impractical
;; here); (x-files packages slojka)'s LD_LIBRARY_PATH wrapper trick
;; backstops those at runtime instead, so #:validate-runpath? is off, same
;; as slojka.
(define %max-libs
  '("mesa" "libxkbcommon" "glib" "dbus" "nss" "nspr" "fontconfig" "expat"
    "alsa-lib" "zlib" "libnotify" "libva" "libvdpau"
    "libgcrypt" "gdk-pixbuf" "pulseaudio"
    "libx11" "libxcomposite" "libxdamage" "libxext" "libxfixes" "libxrandr"
    "libxtst" "libxrender" "libxi" "libxinerama" "libxcursor" "libxau"
    "libxdmcp" "libice" "libsm" "libxmu" "libxaw" "libxpm" "libxres" "libxscrnsaver"
    "libxxf86vm" "libxkbfile" "libfontenc" "libxv" "libxt" "libxcb"
    "xcb-util" "xcb-util-image" "xcb-util-keysyms" "xcb-util-wm"
    "xcb-util-renderutil" "xcb-util-cursor"
    "bzip2" "cairo" "libffi" "gtk+" "pango" "pcre2"))

;; util-linux is a multi-output package -- libmount.so.1 lives only in its
;; "lib" output, not the default "out" (which holds the CLI tools); pulling
;; it in via %max-libs' plain specification->package would silently resolve
;; to a store path with no lib/libmount.so.1 in it at all.  The (package
;; "output") 2-list form is what the package `inputs' field itself wants;
;; file-append inside a gexp needs the same output wrapped as a proper
;; file-like object instead, via gexp-input.
(define %util-linux-package (specification->package "util-linux"))
(define %util-linux-lib-input (list %util-linux-package "lib"))
(define %util-linux-lib (gexp-input %util-linux-package "lib"))

(define %max-version "26.29.0")
(define %max-build "77636")
(define %max-full-version (string-append %max-version "." %max-build))

;; Shell wrapper for the bin/max launcher.  Format args: lib-paths-joined,
;; xdg-data-dirs, real-binary-path.  QTWEBENGINE_DISABLE_SANDBOX is Qt
;; WebEngine's (i.e. its embedded Chromium's) own documented escape hatch
;; for environments with no setuid chrome-sandbox helper -- same problem,
;; same class of fix, as (x-files packages kadr)/slojka's
;; --no-sandbox/--disable-gpu-sandbox flags for their Electron/Chromium.
(define %max-wrapper-script
  "#!/bin/sh
if [ -z \"$DISPLAY\" ]; then
  DISPLAY=:0
fi
if [ -z \"$XAUTHORITY\" ]; then
  XAUTHORITY=$(ls /run/user/$(id -u)/.mutter-Xwaylandauth.* 2>/dev/null | head -1)
fi
exec env \\
  DISPLAY=\"$DISPLAY\" \\
  XAUTHORITY=\"$XAUTHORITY\" \\
  LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\" \\
  XDG_DATA_DIRS=\"~a${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}\" \\
  QTWEBENGINE_DISABLE_SANDBOX=1 \\
  ~a \"$@\"
")

;; Desktop entry as guile-ini data, written from scratch rather than reusing
;; the .deb's own usr/share/applications/max.desktop, which hardcodes
;; /usr/share/max/bin/max and /usr/share/pixmaps/max.png -- both FHS-
;; absolute and wrong once installed into the store.  DBusActivatable and
;; the X-GNOME-* hints are dropped too: they refer to a D-Bus service file
;; this .deb never ships.
(define %max-desktop-entry
  '(("Desktop Entry"
     ("Type"           . "Application")
     ("Name"           . "MAX")
     ("GenericName"    . "Messenger")
     ("Comment"        . "MAX Messenger Client Application")
     ("Comment[ru]"    . "Клиентское приложение мессенджера MAX")
     ("Icon"           . "max")
     ("TryExec"        . "max")
     ("Exec"           . "max %U")
     ("Terminal"       . "false")
     ("MimeType"       . "x-scheme-handler/max;")
     ("StartupWMClass" . "max")
     ("Categories"     . "Chat;Network;InstantMessaging;Qt;"))))

(define-public max-messenger
  (package
    (name "max-messenger")
    (version %max-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.max.ru/linux/deb/pool/main/m/max/MAX-"
                           %max-full-version ".deb"))
       (sha256
        (base32 "0b6a27vjsyrhaxd83gk60aqqq7pjpxazjxilvrf8il4rk1imb1hx"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:install-plan
      #~'(("usr/share/max"     "share/max")
         ("usr/share/icons"   "share/icons")
         ("usr/share/pixmaps" "share/pixmaps"))
      #:phases
      (with-extensions %max-guile-ini-extensions
       #~(modify-phases %standard-phases
          (add-before 'install 'patch-max-binaries
            (lambda _
              (let* ((interpreter
                      #$(file-append (specification->package "glibc")
                                     "/lib/ld-linux-x86-64.so.2"))
                     (abi-compat (string-append #$output "/share/max/abi-compat"))
                     (system-libs
                      (string-join
                       (append
                        (list #$(file-append (this-package-input "nss") "/lib/nss")
                              #$(file-append %util-linux-lib "/lib"))
                        (list #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      %max-libs)))
                       ":"))
                     (max-rpath
                      (string-append abi-compat ":"
                                     #$output "/share/max/lib64:"
                                     #$output "/share/max/lib:"
                                     system-libs))
                     (max-service-rpath
                      (string-append abi-compat ":"
                                     #$output "/share/max/bin/max-service/lib64:"
                                     system-libs)))
                (for-each (lambda (binary)
                            (invoke "patchelf" "--set-interpreter" interpreter binary))
                          '("usr/share/max/bin/max"
                            "usr/share/max/bin/crashpad_handler"
                            "usr/share/max/bin/max-service/bin/max-service"
                            "usr/share/max/bin/max-service/bin/crashpad_handler"
                            "usr/share/max/libexec/QtWebEngineProcess"))
                (invoke "patchelf" "--set-rpath" max-rpath "usr/share/max/bin/max")
                (invoke "patchelf" "--set-rpath" max-service-rpath
                        "usr/share/max/bin/max-service/bin/max-service")
                (invoke "patchelf" "--set-rpath" max-rpath
                        "usr/share/max/libexec/QtWebEngineProcess"))))
          ;; SONAME compatibility shims, confirmed necessary by actually
          ;; launching the built package and iterating on what crashed:
          ;;
          ;; - Guix's mesa is built without glvnd's vendor split --
          ;;   libGL.so.1 alone exports both the glX* (GLX window-system)
          ;;   and gl* (OpenGL core/compat) symbol sets that upstream's
          ;;   Debian build expects split across separate
          ;;   libGLX.so.0/libOpenGL.so.0 SONAMEs (the glvnd convention).
          ;;   There is no glvnd vendor JSON on Guix's non-FHS layout for
          ;;   those SONAMEs to dispatch through either, so simply adding
          ;;   "libglvnd" as an input resolves to a dispatcher with nothing
          ;;   to dispatch to: GLX context creation fails ("Could not
          ;;   initialize GLX") the moment the QML scene needs a real GL
          ;;   surface.  Symlinking both expected SONAMEs directly at
          ;;   mesa's own libGL.so.1 sidesteps glvnd entirely.
          ;; - libffi and pcre2's SONAMEs have simply moved on (.so.6->.so.8,
          ;;   .so.2->.so.3) since whatever Debian build max-service links
          ;;   against; both libraries' relevant ABI surface (libffi's core
          ;;   calling-convention entry points; pcre2-posix's regcomp/regexec
          ;;   wrappers) has been stable across those bumps, so a plain
          ;;   SONAME symlink is enough -- no vendoring an old build needed.
          (add-after 'install 'add-abi-compat-symlinks
            (lambda _
              (let ((dir (string-append #$output "/share/max/abi-compat")))
                (mkdir-p dir)
                (for-each
                 (lambda (target-link)
                   (symlink (car target-link) (string-append dir "/" (cdr target-link))))
                 (list (cons #$(file-append (this-package-input "mesa") "/lib/libGL.so.1")
                             "libGLX.so.0")
                       (cons #$(file-append (this-package-input "mesa") "/lib/libGL.so.1")
                             "libOpenGL.so.0")
                       (cons #$(file-append (this-package-input "libffi") "/lib/libffi.so.8")
                             "libffi.so.6")
                       (cons #$(file-append (this-package-input "pcre2") "/lib/libpcre2-posix.so.3")
                             "libpcre2-posix.so.2"))))))
          (add-after 'install 'install-launcher-and-desktop-entry
            (lambda _
              (use-modules (ini))
              (let ((bin  (string-append #$output "/bin"))
                    (apps (string-append #$output "/share/applications")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/max")
                  (lambda (port)
                    (format port #$%max-wrapper-script
                            (string-join
                             (append
                              (list (string-append #$output "/share/max/abi-compat")
                                    (string-append #$output "/share/max/lib64")
                                    (string-append #$output "/share/max/lib")
                                    #$(file-append (this-package-input "nss") "/lib/nss")
                                    #$(file-append %util-linux-lib "/lib"))
                              (list #$@(map (lambda (pkg)
                                              (file-append (this-package-input pkg) "/lib"))
                                            %max-libs)))
                             ":")
                            (string-append
                             #$(this-package-input "gsettings-desktop-schemas")
                             "/share" ":" #$output "/share")
                            (string-append #$output "/share/max/bin/max"))))
                (chmod (string-append bin "/max") #o755)

                (mkdir-p apps)
                (call-with-output-file (string-append apps "/max.desktop")
                  (lambda (port)
                    (scm->ini '#$%max-desktop-entry #:port port))))))))))
    (inputs
     (cons* (specification->package "gsettings-desktop-schemas")
           %util-linux-lib-input
           (map specification->package %max-libs)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://max.ru")
    (synopsis "Desktop client for the MAX messenger")
    (description
     "MAX is a Russian instant-messaging and calling client, built on Qt6/QML
with an embedded QtWebEngine (Chromium) for its mini-apps and a bundled
@code{max-service} background helper for calls and notifications.  This
package installs the upstream prebuilt Debian package.")
    (license (nonfree "https://legal.max.ru/ps"))))
