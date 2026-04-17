(define-module (x-files packages spotify)
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
  #:use-module ((gnu packages compression)     #:select (squashfs-tools)))

(define %spotify-desktop-entry
  "[Desktop Entry]
Type=Application
Name=Spotify
GenericName=Music Player
Icon=spotify
TryExec=spotify
Exec=spotify %U
Terminal=false
MimeType=x-scheme-handler/spotify;
Categories=Audio;Music;Player;AudioVideo;
StartupWMClass=spotify
")

;; Shell wrapper template for the bin/spotify launcher.
;; Format args: app-dir, ":", lib-paths-joined, fontconfig-conf, binary-path.
(define %spotify-wrapper-script
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
  LD_LIBRARY_PATH=\"~a~a~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\" \\
  FONTCONFIG_FILE=\"~a\" \\
  ~a --no-sandbox --disable-gpu-sandbox --disable-dev-shm-usage \"$@\"
")

;; Input names used in both #:patchelf-plan and LD_LIBRARY_PATH wrapping.
(define %spotify-libs
  '("alsa-lib" "at-spi2-core" "cairo" "cups" "dbus"
    "expat" "gdk-pixbuf" "glib" "gtk+" "harfbuzz" "libice" "libsm"
    "libnotify" "libx11" "libxcb" "libxcomposite" "libxcursor" "libxdamage"
    "libxext" "libxfixes" "libxi" "libxkbcommon" "libxrandr" "libxrender"
    "libxscrnsaver" "libxshmfence" "libxtst" "mesa" "nspr" "pango"
    "gcc-toolchain" "eudev" "zlib"
    "libappindicator" "libdbusmenu" "fontconfig"))

(define-public spotify
  (package
    (name "spotify")
    (version "1.2.74.477.g3be53afe")
    (source
     (origin
       (method url-fetch)
       ;; Snapcraft provides indefinite version history; Debian repos only keep
       ;; the last two releases.
       (uri "https://api.snapcraft.io/api/v1/snaps/download/pOBIoZ2LrCB3rDohMxoYGnbN14EHOgD7_89.snap")
       (sha256
        (base32 "05pzcw4ckjn8j3cyab0wlbk8h15i6dq1nlvvak94dgh9bvlfmaq4"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:validate-runpath? #f
      #:substitutable? #f
      ;; Patch system library rpaths into the main binary.
      #:patchelf-plan `'(("squashfs-root/usr/share/spotify/spotify"
                          ,%spotify-libs))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "unsquashfs" source)))

          (add-before 'patchelf 'make-binary-writable
            (lambda _
              (chmod "squashfs-root/usr/share/spotify/spotify" #o755)))

          (replace 'install
            (lambda _
              (let* ((app   (string-append #$output "/share/spotify"))
                     (bin   (string-append #$output "/bin"))
                     (icons (string-append #$output "/share/icons/hicolor"))
                     (apps  (string-append #$output "/share/applications")))

                ;; Application tree (includes bundled libcef.so etc.)
                (copy-recursively "squashfs-root/usr/share/spotify" app)
                (chmod (string-append app "/spotify") #o755)

                ;; libayatana-appindicator3 is not in Guix; provide a compat
                ;; symlink so Spotify's system-tray code can dlopen it via
                ;; the ABI-compatible libappindicator3.
                (symlink (string-append #$(this-package-input "libappindicator")
                                        "/lib/libappindicator3.so.1")
                         (string-append app "/libayatana-appindicator3.so.1"))

                ;; bin/ wrapper: prepend app dir to LD_LIBRARY_PATH so the
                ;; bundled libcef.so and friends are found, then exec the binary.
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/spotify")
                  (lambda (port)
                    (format port #$%spotify-wrapper-script
                            app ":"
                            (string-join
                             (list
                              (string-append #$(this-package-input "nss") "/lib/nss")
                              #$@(map (lambda (pkg)
                                        (file-append (this-package-input pkg) "/lib"))
                                      %spotify-libs))
                             ":")
                            (string-append #$(this-package-input "fontconfig")
                                           "/etc/fonts/fonts.conf")
                            (string-append app "/spotify"))))
                (chmod (string-append bin "/spotify") #o755)

                ;; Icons
                (for-each
                 (lambda (size)
                   (let ((src (string-append app "/icons/spotify-linux-"
                                             size ".png"))
                         (dir (string-append icons "/" size "x" size "/apps")))
                     (when (file-exists? src)
                       (mkdir-p dir)
                       (copy-file src (string-append dir "/spotify.png")))))
                 '("16" "22" "24" "32" "48" "64" "128" "256" "512"))

                ;; Desktop entry
                (mkdir-p apps)
                (call-with-output-file (string-append apps "/spotify.desktop")
                  (lambda (port)
                    (display #$%spotify-desktop-entry port)))))))))
    (native-inputs (list squashfs-tools))
    (inputs
     (map specification->package
          (append %spotify-libs (list "nss"))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://www.spotify.com/")
    (synopsis "Music streaming service client")
    (description
     "Spotify is a digital music service giving access to millions of songs.
This package provides the official Spotify desktop client, sourced from the
Snapcraft store.")
    (license (nonfree "https://www.spotify.com/legal/end-user-agreement/"))))
