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
  #:use-module ((guix build-system copy)   #:select (copy-build-system))
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
  #:use-module ((gnu packages linux)       #:select (eudev libcap numactl pipewire))
  #:use-module ((gnu packages pkg-config)  #:select (pkg-config))
  #:use-module ((gnu packages pulseaudio)  #:select (pulseaudio))
  #:use-module ((gnu packages python)      #:select (python))
  #:use-module ((gnu packages python-xyz)  #:select (python-jinja2))
  #:use-module ((gnu packages python-build) #:select (python-setuptools))
  #:use-module ((gnu packages tls)         #:select (openssl))
  #:use-module ((gnu packages upnp)        #:select (miniupnpc))
  #:use-module ((gnu packages video)       #:select (libva libvdpau))
  #:use-module ((gnu packages vulkan)      #:select (shaderc vulkan-loader))
  #:use-module ((gnu packages xdisorg)     #:select (libdrm))
  #:use-module ((gnu packages xiph)        #:select (opus))
  #:use-module ((gnu packages xorg)        #:select (libevdev libx11 libxcb
                                                     libxfixes libxrandr libxtst)))

;; Since v2026 Sunshine no longer builds FFmpeg from source: cmake/dependencies/
;; ffmpeg.cmake downloads LizardByte's own prebuilt static libraries (libavcodec.a,
;; libcbs.a, libSvtAv1Enc.a, libx264.a, libx265.a, ...) from their `build-deps'
;; GitHub releases at configure time.  There is no network in the Guix build
;; sandbox, so we fetch that same prebuilt artifact ourselves and hand it to cmake
;; via -DFFMPEG_PREPARED_BINARIES.  The `build-deps' release tag is the one the
;; Sunshine tag pins as its `third-party/build-deps' submodule commit
;; (fce763b -> v2026.516.30821).
(define sunshine-ffmpeg
  (package
    (name "sunshine-ffmpeg-prebuilt")
    (version "2026.516.30821")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/LizardByte/build-deps/releases/download/v"
                                  version "/Linux-x86_64-ffmpeg.tar.gz"))
              (file-name (string-append "sunshine-ffmpeg-" version ".tar.gz"))
              (sha256 (base32 "1v667cf34scrkh1gkzhvkhx8qdsq796npqiszzlfyyw6qby1j8y3"))))
    (build-system copy-build-system)
    (arguments
     ;; Prebuilt static archives + headers: do not strip or patch, just relocate
     ;; the extracted `ffmpeg/' tree (lib/, include/) into the store unchanged.
     (list #:strip-binaries? #f
           #:install-plan #~'(("." "ffmpeg/"))))
    (home-page "https://github.com/LizardByte/build-deps")
    (synopsis "Prebuilt static FFmpeg libraries for Sunshine")
    (description "LizardByte's prebuilt static FFmpeg build (with x264, x265,
SvtAv1 and the coded-bitstream library) that Sunshine links against since v2026.
Provided here so the Guix build does not have to download it at configure time.")
    (license license:gpl2+)))

(define-public sunshine
  (package
    (name "sunshine")
    (version "2026.516.143833")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LizardByte/Sunshine.git")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32 "1b87qnwmwycz5w9avg85jiwi346fk4yx6y6nfpwaqimm4lxs2ayz"))))
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
         ;; Use the prebuilt static FFmpeg instead of downloading it.
         (string-append "-DFFMPEG_PREPARED_BINARIES="
                        (assoc-ref %build-inputs "sunshine-ffmpeg") "/ffmpeg")
         ;; glad's GL loader is generated at build time by a Python/jinja2
         ;; script.  Point it at our Python (which has jinja2 on
         ;; GUIX_PYTHONPATH) and forbid it from pip-installing anything.
         "-DGLAD_SKIP_PIP_INSTALL=ON"
         (string-append "-DPython_EXECUTABLE="
                        (assoc-ref %build-inputs "python") "/bin/python3")
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
      boost
      libcap
      curl
      libdrm
      libevdev
      miniupnpc
      libnotify
      numactl
      opus
      ;; Wayland screencast (XDG desktop portal / KWin ScreenCast) since v2026.
      pipewire
      pulseaudio
      openssl
      libva
      libvdpau
      ;; Vulkan video encoding (via the prebuilt FFmpeg) is enabled by default
      ;; since v2026; the loader provides libvulkan.so linked into sunshine.
      vulkan-loader
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
       ;; Prebuilt static FFmpeg (see sunshine-ffmpeg above), consumed via
       ;; -DFFMPEG_PREPARED_BINARIES.
       ("sunshine-ffmpeg" ,sunshine-ffmpeg)
       ;; glad's GL-loader generator runs at build time and needs jinja2.
       ("python" ,python)
       ("python-jinja2" ,python-jinja2)
       ("python-setuptools" ,python-setuptools)
       ;; Compiles the Vulkan encode shaders at build time (cmake prefers glslc).
       ("shaderc" ,shaderc)
       ;; LizardByte's official Arch package, used only for its prebuilt
       ;; Vite web UI (usr/share/sunshine/web); replaces forgoty's
       ;; third-party npm-offline-cache.
       ("sunshine-web-ui"
        ,(origin
           (method url-fetch)
           ;; Upstream dropped the generic `sunshine.pkg.tar.zst' asset; the
           ;; built Arch binary package (which carries usr/share/sunshine/web)
           ;; is now versioned and arch-tagged.  The `sunshine.pkg.tar.gz'
           ;; asset is only the PKGBUILD source, so it is not usable here.
           (uri (string-append "https://github.com/LizardByte/Sunshine/releases/download/v"
                               version "/sunshine-" version "-1-x86_64.pkg.tar.zst"))
           (file-name (string-append "sunshine-web-ui-" version ".pkg.tar.zst"))
           (sha256 (base32 "16ajzidb4z2pnjx5c2dybbr5pk9r5kbqa6q0www1zy3rn37kjrb7"))))))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description "Sunshine is a self-hosted game stream host for Moonlight. Offering low latency, cloud gaming server capabilities with support for AMD, Intel, and Nvidia GPUs for hardware encoding. Software encoding is also available.")
    (license license:gpl3)))

sunshine
