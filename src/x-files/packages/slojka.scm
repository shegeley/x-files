(define-module (x-files packages slojka)
  #:use-module (guix gexp)
  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)               #:select (package
                                                        origin
                                                        base32
                                                        this-package-input))
  #:use-module ((gnu packages)                #:select (specification->package))
  #:use-module ((guix download)               #:select (url-fetch))
  #:use-module ((nonguix build-system binary)  #:select (binary-build-system))
  #:use-module ((gnu packages compression)     #:select (squashfs-tools))
  #:use-module ((gnu packages guile-xyz)       #:select (guile-ini
                                                        guile-smc
                                                        guile-lib))
  #:use-module ((gnu packages imagemagick)     #:select (imagemagick)))

;; guile-ini's (ini) module pulls in (ini fsm), which needs guile-smc and
;; guile-lib; all three must be on the builder's load path.  Same set as
;; (x-files services dconf).
(define %guile-ini-extensions (list guile-ini guile-smc guile-lib))

;; Слойка ships only as a prebuilt Electron AppImage (electron-builder 26).
;; Building from source is impractical under Guix: an npm-workspaces monorepo
;; with a WebGL2 engine, onnxruntime-web and a Python/PyTorch sidecar, none of
;; which Guix can build from the npm/pip registries.  So we unpack the AppImage
;; (a squashfs image with an ELF runtime prepended) and patchelf/LD-wrap the
;; Electron binary — same recipe as (x-files packages spotify).
;;
;; Only the editor core is packaged.  The AI subsystems (SAM 2.1, polza.ai) are
;; strictly optional and provision themselves into a user venv at
;; ~/.local/share/slojka/venv at runtime; background removal runs on bundled
;; ONNX-WASM.  The editor launches fully without any Python.

;; Desktop entry as guile-ini data: an alist of sections, each a pair of a
;; section title and its (key . value) properties.  scm->ini serialises this to
;; the "[Section]\nKey=Value" form (no spaces around "="), exactly what the
;; freedesktop Desktop Entry spec wants.
(define %slojka-desktop-entry
  '(("Desktop Entry"
     ("Type"          . "Application")
     ("Name"          . "Слойка")
     ("GenericName"   . "Graphics Editor")
     ("Comment"       . "AI-first graphics editor")
     ("Icon"          . "slojka")
     ("TryExec"       . "slojka")
     ("Exec"          . "slojka %U")
     ("Terminal"      . "false")
     ("Categories"    . "Graphics;2DGraphics;RasterGraphics;")
     ("StartupWMClass" . "Slojka"))))

;; Icon sizes rendered from the single 512x512 PNG the AppImage ships, so the
;; hicolor theme has a full ladder for panels, docks and the app switcher.
(define %slojka-icon-sizes '("16" "24" "32" "48" "64" "128" "256" "512"))

;; Shell wrapper template for the bin/slojka launcher.
;; Format args: lib-paths-joined, fontconfig-conf, binary-path.
(define %slojka-wrapper-script
  "#!/bin/sh
# Under Wayland, DISPLAY and XAUTHORITY are not exported to terminals.
# Auto-detect them from the running XWayland instance.
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
;; Derived from `patchelf --print-needed` on the Electron binary and its
;; bundled ANGLE/SwiftShader libraries.
(define %slojka-libs
  '("alsa-lib" "at-spi2-core" "cairo" "cups" "dbus"
    "expat" "gdk-pixbuf" "glib" "gtk+" "harfbuzz"
    "libx11" "libxcb" "libxcomposite" "libxdamage" "libxext"
    "libxfixes" "libxkbcommon" "libxrandr"
    "mesa" "nspr" "pango" "gcc-toolchain" "eudev" "zlib" "fontconfig"))

(define-public slojka
  (package
    (name "slojka")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/HelpFreedom/slojka/releases/download/v"
                           version "/Slojka-" version ".AppImage"))
       (sha256
        (base32 "0vg5j3b8n12784ifcv07z027slzzbrm08y9vfjybpmjskb37cfbb"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:substitutable? #f
      ;; Patch the ELF interpreter and system-library rpaths into the two
      ;; ELF executables (the Electron main binary and the crashpad handler).
      #:patchelf-plan
      `'(("squashfs-root/slojka"                  ,%slojka-libs)
         ("squashfs-root/chrome_crashpad_handler" ,%slojka-libs))
      ;; with-extensions puts guile-ini's (ini) module on the builder's load
      ;; path (with guile-smc/guile-lib it depends on), so the install phase can
      ;; serialise the desktop entry with scm->ini.
      #:phases
      (with-extensions %guile-ini-extensions
       #~(modify-phases %standard-phases
          ;; The AppImage is an ELF runtime with a squashfs image appended.
          ;; The image starts right after the runtime, i.e. at the end of the
          ;; ELF section-header table: e_shoff + e_shentsize * e_shnum.
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (let* ((get-bytevector-n (@ (ice-9 binary-ports) get-bytevector-n))
                     (bytevector-u8-ref (@ (rnrs bytevectors) bytevector-u8-ref))
                     (hdr (call-with-input-file source
                            (lambda (p) (get-bytevector-n p 64))))
                     (u (lambda (start n)
                          (let loop ((i 0) (acc 0))
                            (if (= i n)
                                acc
                                (loop (+ i 1)
                                      (logior acc
                                              (ash (bytevector-u8-ref hdr (+ start i))
                                                   (* i 8))))))))
                     (offset (+ (u 40 8) (* (u 58 2) (u 60 2)))))
                (invoke "unsquashfs" "-o" (number->string offset) source))))

          (add-before 'patchelf 'make-binaries-writable
            (lambda _
              (for-each (lambda (f) (chmod f #o755))
                        '("squashfs-root/slojka"
                          "squashfs-root/chrome_crashpad_handler"))))

          (replace 'install
            (lambda _
              (use-modules (ini))
              (let* ((app     (string-append #$output "/share/slojka"))
                     (bin     (string-append #$output "/bin"))
                     (hicolor (string-append #$output "/share/icons/hicolor"))
                     (apps    (string-append #$output "/share/applications")))

                ;; Whole application tree (Electron binary, resources.pak,
                ;; app.asar, locales, bundled libEGL/libGLESv2/libffmpeg, and
                ;; the usr/lib tray indicator libs).
                (copy-recursively "squashfs-root" app)
                (chmod (string-append app "/slojka") #o755)

                ;; bin/ wrapper: prepend the app dir (bundled .so's) and its
                ;; usr/lib (indicator libs), then the Guix system libraries and
                ;; the special nss/lib/nss path, to LD_LIBRARY_PATH.
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/slojka")
                  (lambda (port)
                    (format port #$%slojka-wrapper-script
                            (string-join
                             (append
                              (list app
                                    (string-append app "/usr/lib")
                                    (string-append
                                     #$(this-package-input "nss") "/lib/nss"))
                              (list #$@(map (lambda (pkg)
                                              (file-append
                                               (this-package-input pkg) "/lib"))
                                            %slojka-libs)))
                             ":")
                            (string-append #$(this-package-input "fontconfig")
                                           "/etc/fonts/fonts.conf")
                            (string-append app "/slojka"))))
                (chmod (string-append bin "/slojka") #o755)

                ;; Icons: the AppImage ships only a 512x512 PNG; render the
                ;; smaller hicolor sizes from it with ImageMagick.
                (let ((src (string-append
                            app "/usr/share/icons/hicolor/512x512/apps/slojka.png")))
                  (for-each
                   (lambda (size)
                     (let ((dir (string-append hicolor "/" size "x" size "/apps")))
                       (mkdir-p dir)
                       (if (string=? size "512")
                           (copy-file src (string-append dir "/slojka.png"))
                           (invoke #$(file-append imagemagick "/bin/convert")
                                   src "-resize" (string-append size "x" size)
                                   (string-append dir "/slojka.png")))))
                   '#$%slojka-icon-sizes))

                ;; Desktop entry, serialised from guile-ini data.
                (mkdir-p apps)
                (call-with-output-file (string-append apps "/slojka.desktop")
                  (lambda (port)
                    (scm->ini '#$%slojka-desktop-entry #:port port))))))))))
    (native-inputs (list squashfs-tools imagemagick))
    (inputs
     (map specification->package
          (append %slojka-libs (list "nss"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/HelpFreedom/slojka")
    (synopsis "AI-first raster graphics editor")
    (description
     "Слойка (Slojka) is an AI-integrated raster graphics editor built on
Electron and a WebGL2 compositor.  It provides layers (including smart
layers), masks, selections, brushes and text styling.  Optional AI features
integrate local SAM 2.1 segmentation, the polza.ai image-generation hub and
Claude Code as an assistant that drives the editor over MCP; these provision
themselves into a per-user virtualenv at runtime and are not required for the
editor to run.  This package installs the upstream prebuilt AppImage.")
    (license license:gpl3)))

slojka
