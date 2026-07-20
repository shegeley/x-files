(define-module (x-files packages sunshine)
  ;; Originally appropriated from forgoty's channel:
  ;;   https://github.com/forgoty/dotfiles/blob/master/guix/forgoty/packages/streaming.scm
  ;; Since diverged: forgoty builds the Vite/Vue web UI from a third-party
  ;; npm-offline-cache release.  We dropped npm entirely and instead install
  ;; the web UI assets from LizardByte's own official release package.
  #:use-module (guix gexp)
  #:use-module ((guix packages)            #:select (package origin base32))
  #:use-module ((guix git-download)        #:select (git-fetch git-reference))
  #:use-module ((guix download)            #:select (url-fetch))
  #:use-module ((guix build-system cmake)  #:select (cmake-build-system))
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module ((gnu packages avahi)       #:select (avahi))
  #:use-module ((gnu packages base)        #:select (tar))
  #:use-module ((gnu packages compression) #:select (zstd))
  #:use-module ((gnu packages boost)       #:select (boost))
  #:use-module ((gnu packages cpp)         #:select (nlohmann-json))
  #:use-module ((gnu packages curl)        #:select (curl))
  #:use-module ((gnu packages freedesktop) #:select (libappindicator wayland))
  #:use-module ((gnu packages gl)          #:select (mesa))
  #:use-module ((gnu packages gnome)       #:select (libnotify))
  #:use-module ((gnu packages linux)       #:select (eudev libcap numactl))
  #:use-module ((gnu packages pkg-config)  #:select (pkg-config))
  #:use-module ((gnu packages pulseaudio)  #:select (pulseaudio))
  #:use-module ((gnu packages tls)         #:select (openssl))
  #:use-module ((gnu packages upnp)        #:select (miniupnpc))
  #:use-module ((gnu packages video)       #:select (libva libvdpau))
  #:use-module ((gnu packages xdisorg)     #:select (libdrm))
  #:use-module ((gnu packages xiph)        #:select (opus))
  #:use-module ((gnu packages xorg)        #:select (libevdev libx11 libxcb
                                                     libxfixes libxrandr libxtst)))

(define (version-with-underscores version)
  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version))

(define-public boost-1.87
  (package
    (inherit boost)
    (version "1.87.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archives.boost.io/release/"
                                  version "/source/boost_"
                                  (version-with-underscores version) ".tar.bz2"))
              (sha256
               (base32
                "12bxa96qym7g2552kghgllp3bd7zi8vzx4nn7r0lnkscrcjvwmxg"))))))

(define-public sunshine
  (package
    (name "sunshine")
    (version "2025.924.154138")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LizardByte/Sunshine.git")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32 "13v7cg0lm14n3pblmmbqn2zp999vx5acc8qmlaghp9kxlxkdzcs2"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
	   #:configure-flags
	   #~(list
	       "-Wno-dev"
	       "-DBOOST_USE_STATIC=false"
	       "-DSUNSHINE_ENABLE_CUDA=OFF"
	       "-DBUILD_DOCS=false"
	       "-DBUILD_TESTS=OFF"
         (string-append "-DOPENSSL_ROOT_DIR=" (assoc-ref %build-inputs "openssl")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'modify-src
            (lambda _
              (substitute* "cmake/packaging/linux.cmake"
                (("\\$\\{UDEV_RULES_INSTALL_DIR\\}")
                  (string-append #$output "/lib/udev/rules.d"))
                (("\\$\\{SYSTEMD_USER_UNIT_INSTALL_DIR\\}")
                 "${SUNSHINE_ASSETS_DIR}/systemd/user"))
              (substitute* "src/platform/linux/publish.cpp"
                          (("libavahi-(common|client)\\.so" all)
                            (string-append #$avahi "/lib/" all)))
              (substitute* "src/platform/linux/x11grab.cpp"
                (("libXrandr\\.so" all)
                (string-append #$libxrandr "/lib/" all))
                (("libXfixes\\.so" all)
                (string-append #$libxfixes "/lib/" all))
                (("libX11\\.so" all)
                (string-append #$libx11 "/lib/" all))
                (("libxcb(-shm|)\\.so" all)
                 (string-append #$libxcb "/lib/" all)))))
          ;; The web UI is a Vite/Vue build that would require a whole npm
          ;; dependency tree.  We install LizardByte's own prebuilt assets
          ;; instead (see 'install-prebuilt-web-ui), so excise the npm-driven
          ;; `web-ui' target from the build entirely: no npm is probed or run.
          (add-after 'unpack 'remove-web-ui-npm-build
            (lambda _
              (let* ((file  "cmake/targets/common.cmake")
                     (text  (call-with-input-file file
                              (@ (ice-9 textual-ports) get-string-all)))
                     (start (string-contains text "find_program(NPM npm"))
                     (mark  (string-contains text "VERBATIM)" start))
                     (end   (+ mark (string-length "VERBATIM)"))))
                (call-with-output-file file
                  (lambda (port)
                    ((@ (ice-9 textual-ports) put-string) port
                     (string-append
                      (substring text 0 start)
                      "# npm web-ui build removed; prebuilt assets installed instead."
                      (substring text end))))))))
          (add-before 'configure 'set-version
            (lambda _
              (setenv "BRANCH" (string-append "v" #$version))
              (setenv "BUILD_VERSION" #$version)))
          ;; Place LizardByte's officially-built Vite web UI where CMake's
          ;; `install(DIRECTORY ${CMAKE_BINARY_DIR}/assets/web ...)` expects it.
          (add-before 'install 'install-prebuilt-web-ui
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir-p "assets")
              (invoke "tar" "--zstd" "-xf"
                      (assoc-ref inputs "sunshine-web-ui")
                      "usr/share/sunshine/web")
              (copy-recursively "usr/share/sunshine/web" "assets/web")
              (delete-file-recursively "usr"))))))
    (inputs
     (list
      eudev
      libappindicator
      boost-1.87
      libcap
      curl
      libdrm
      libevdev
      miniupnpc
      libnotify
      numactl
      opus
      pulseaudio
      openssl
      libva
      libvdpau
      wayland
      libx11
      libxtst
      libxrandr
      libxfixes
      libxcb
      nlohmann-json
      mesa
      avahi))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("tar" ,tar)
       ("zstd" ,zstd)
       ;; LizardByte's official Arch package, used only for its prebuilt
       ;; Vite web UI (usr/share/sunshine/web); replaces forgoty's
       ;; third-party npm-offline-cache.
       ("sunshine-web-ui"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://github.com/LizardByte/Sunshine/releases/download/v"
                               version "/sunshine.pkg.tar.zst"))
           (file-name (string-append "sunshine-web-ui-" version ".pkg.tar.zst"))
           (sha256 (base32 "0g4wzpdcm0p7slf4ih4ly6skslcb2nsys0pihrn39yz6zlgy0yrd"))))))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description "Sunshine is a self-hosted game stream host for Moonlight. Offering low latency, cloud gaming server capabilities with support for AMD, Intel, and Nvidia GPUs for hardware encoding. Software encoding is also available.")
    (license license:gpl3)))

sunshine
