(define-module (x-files packages postman)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)               #:select (package
                                                        origin
                                                        base32
                                                        this-package-input))
  #:use-module ((gnu packages)                #:select (specification->package))
  #:use-module ((guix download)               #:select (url-fetch))
  #:use-module ((nonguix build-system binary)  #:select (binary-build-system))
  #:use-module ((nonguix licenses)             #:select (nonfree))
  #:use-module ((gnu packages imagemagick)     #:select (imagemagick))
  #:use-module ((gnu packages guile-xyz)       #:select (guile-ini
                                                        guile-smc
                                                        guile-lib)))

;; Postman (postman.com) is an Electron app, shipped upstream only as a
;; prebuilt tarball (https://dl.pstmn.io/download/version/VERSION/linux64,
;; same content as the "latest" alias but pinned to a fixed version for
;; reproducibility) -- same binary-build-system approach as
;; (x-files packages spotify)/slojka, but simpler still: unlike their
;; snap/AppImage sources, a plain .tar.gz is a format binary-build-system's
;; inherited (from gnu-build-system) `unpack' phase already understands
;; natively, so no custom unpack phase is needed here either.
;;
;; guile-ini's (ini) module pulls in (ini fsm), which needs guile-smc and
;; guile-lib; all three must be on the builder's load path.  Same set as
;; (x-files services dconf).
(define %guile-ini-extensions (list guile-ini guile-smc guile-lib))

;; Desktop entry as guile-ini data: (("Section" ("Key" . "Value") ...)).
;; scm->ini serialises it to the freedesktop "[Section]\nKey=Value" form.
;; Postman ships no .desktop file of its own.
(define %postman-desktop-entry
  '(("Desktop Entry"
     ("Type"          . "Application")
     ("Name"          . "Postman")
     ("GenericName"   . "API Client")
     ("Comment"       . "Build, test, and document APIs")
     ("Icon"          . "postman")
     ("TryExec"       . "postman")
     ("Exec"          . "postman %U")
     ("Terminal"      . "false")
     ("Categories"    . "Development;")
     ("StartupWMClass" . "Postman"))))

;; Shell wrapper template for the bin/postman launcher.  Chromium's own
;; sandbox setup fails under Guix (no setuid chrome-sandbox helper in the
;; store), and Wayland doesn't export DISPLAY/XAUTHORITY to terminals --
;; same fixes as (x-files packages spotify)/slojka.
(define %postman-wrapper-script
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
  FONTCONFIG_FILE=\"~a\" \\
  ~a --no-sandbox --disable-gpu-sandbox --disable-dev-shm-usage \"$@\"
")

;; Input names used in both #:patchelf-plan and LD_LIBRARY_PATH wrapping.
;; Derived from `patchelf --print-needed' on app/postman and
;; app/chrome_crashpad_handler.
(define %postman-libs
  '("alsa-lib" "at-spi2-core" "cairo" "cups" "dbus" "expat" "glib" "gtk+"
    "pango" "libx11" "libxcomposite" "libxdamage" "libxext" "libxfixes"
    "libxrandr" "mesa" "libxcb" "libxkbcommon" "eudev" "gcc-toolchain"
    "nspr" "fontconfig"))

(define %postman-icon-sizes '("16" "24" "32" "48" "64" "128"))

(define-public postman
  (package
    (name "postman")
    (version "12.24.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.pstmn.io/download/version/"
                           version "/linux64"))
       (file-name (string-append "postman-" version ".tar.gz"))
       (sha256
        (base32 "1b7405k61jhzkizpjr9hsyr0hjs40zi8qx2kcha6ddr1dl91v50m"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:substitutable? #f
      ;; Patch the main Electron binary and the crashpad handler; the tiny
      ;; native "Postman" launcher stub is bypassed entirely (the bin/
      ;; wrapper execs app/postman directly, same as spotify/slojka exec
      ;; their main Electron binary rather than any launcher stub).
      #:patchelf-plan
      `'(("app/postman"                  ,%postman-libs)
         ("app/chrome_crashpad_handler"  ,%postman-libs))
      #:phases
      (with-extensions %guile-ini-extensions
       #~(modify-phases %standard-phases
          (add-before 'patchelf 'make-binaries-writable
            (lambda _
              (for-each (lambda (f) (chmod f #o755))
                        '("app/postman" "app/chrome_crashpad_handler"))))

          (replace 'install
            (lambda _
              (use-modules (ini))
              (let* ((app     (string-append #$output "/share/postman"))
                     (bin     (string-append #$output "/bin"))
                     (hicolor (string-append #$output "/share/icons/hicolor"))
                     (apps    (string-append #$output "/share/applications")))

                ;; Whole application tree (Electron binary, resources.pak,
                ;; app.asar-less resources/, bundled libEGL/libGLESv2/
                ;; libffmpeg, locales).
                (copy-recursively "app" app)
                (chmod (string-append app "/postman") #o755)

                (mkdir-p bin)
                (call-with-output-file (string-append bin "/postman")
                  (lambda (port)
                    (format port #$%postman-wrapper-script
                            (string-join
                             (append
                              ;; app itself first: postman's own rpath is
                              ;; $ORIGIN (bundled libffmpeg.so etc live
                              ;; alongside it), which #:patchelf-plan's
                              ;; --set-rpath overwrites -- restore it via
                              ;; LD_LIBRARY_PATH instead.
                              (list app
                                    (string-append
                                     #$(this-package-input "nss") "/lib/nss"))
                              (list #$@(map (lambda (pkg)
                                              (file-append
                                               (this-package-input pkg) "/lib"))
                                            %postman-libs)))
                             ":")
                            (string-append #$(this-package-input "fontconfig")
                                           "/etc/fonts/fonts.conf")
                            (string-append app "/postman"))))
                (chmod (string-append bin "/postman") #o755)

                ;; Icons: Postman ships a single 128x128 PNG; render the
                ;; smaller hicolor sizes from it with ImageMagick.
                (let ((src (string-append app "/resources/app/assets/icon.png")))
                  (for-each
                   (lambda (size)
                     (let ((dir (string-append hicolor "/" size "x" size "/apps")))
                       (mkdir-p dir)
                       (if (string=? size "128")
                           (copy-file src (string-append dir "/postman.png"))
                           (invoke #$(file-append imagemagick "/bin/convert")
                                   src "-resize" (string-append size "x" size)
                                   (string-append dir "/postman.png")))))
                   '#$%postman-icon-sizes))

                ;; Desktop entry, serialised from guile-ini data.
                (mkdir-p apps)
                (call-with-output-file (string-append apps "/postman.desktop")
                  (lambda (port)
                    (scm->ini '#$%postman-desktop-entry #:port port))))))))))
    (native-inputs (list imagemagick))
    (inputs
     (map specification->package (append %postman-libs (list "nss"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.postman.com/")
    (synopsis "API platform for building and using APIs")
    (description
     "Postman is a desktop API client for building, testing, and
documenting REST, GraphQL, WebSocket, and gRPC APIs.  This package installs
the upstream prebuilt Electron application.")
    (license (nonfree "https://www.postman.com/legal/terms/"))))

postman
