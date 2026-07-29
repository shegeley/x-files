(define-module (x-files packages sunshine)
  ;; Originally appropriated from forgoty's channel:
  ;;   https://github.com/forgoty/dotfiles/blob/master/guix/forgoty/packages/streaming.scm
  ;; Since diverged: forgoty builds the Vite/Vue web UI from a third-party
  ;; npm-offline-cache release.  We dropped npm entirely and instead install
  ;; the web UI assets from LizardByte's own official release package.
  #:use-module (guix gexp)
  #:use-module ((guix packages)            #:select (package origin base32
                                                     package-arguments
                                                     package-source
                                                     package-version))
  #:use-module ((guix git-download)        #:select (git-fetch git-reference))
  #:use-module ((guix download)            #:select (url-fetch))
  #:use-module ((guix build-system cmake)  #:select (cmake-build-system))
  #:use-module ((guix build-system copy)   #:select (copy-build-system))
  #:use-module ((guix build-system gnu)    #:select (gnu-build-system))
  #:use-module ((guix utils)               #:select (substitute-keyword-arguments))
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
  #:use-module ((gnu packages video)       #:select (libva libvdpau
                                                     libx264 x265-4 svt-av1-3
                                                     ffmpeg))
  #:use-module ((gnu packages vulkan)      #:select (shaderc vulkan-loader
                                                     vulkan-headers))
  #:use-module ((gnu packages assembly)    #:select (nasm))
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

;; ---------------------------------------------------------------------------
;; Unvendored FFmpeg: build FFmpeg + libcbs from Guix's own FFmpeg source
;; (8.1.x matches build-deps 8.1.1), linking x264/x265/SVT-AV1 dynamically from
;; Guix, so no prebuilt blob is downloaded.  Assembled into the same
;; `ffmpeg/{lib,include}' layout Sunshine's FFMPEG_PREPARED_BINARIES expects.
;; ---------------------------------------------------------------------------
(define %cbs-source-files
  '("libavcodec/cbs.c" "libavcodec/cbs_h2645.c" "libavcodec/cbs_av1.c"
    "libavcodec/cbs_vp8.c" "libavcodec/cbs_vp9.c" "libavcodec/cbs_mpeg2.c"
    "libavcodec/cbs_jpeg.c" "libavcodec/cbs_sei.c" "libavcodec/h264_levels.c"
    "libavcodec/h2645_parse.c" "libavcodec/vp8data.c" "libavutil/intmath.c"))

(define %cbs-avcodec-headers
  '("av1.h" "cbs_av1.h" "cbs_bsf.h" "cbs.h" "cbs_h2645.h" "cbs_h264.h"
    "cbs_h265.h" "cbs_jpeg.h" "cbs_mpeg2.h" "cbs_sei.h" "cbs_vp8.h" "cbs_vp9.h"
    "codec_desc.h" "codec_id.h" "codec_par.h" "defs.h" "get_bits.h"
    "h264_levels.h" "h2645_parse.h" "h264.h" "mathops.h" "packet.h" "sei.h"
    "version_major.h" "vlc.h"))

(define sunshine-ffmpeg-guix
  (package
    (name "sunshine-ffmpeg-guix")
    (version (package-version ffmpeg))
    (source (package-source ffmpeg))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; build-deps cbs patch 01: the standalone cbs subset lacks the
          ;; transitive include of intmath.h that the full FFmpeg build gets.
          (add-after 'unpack 'cbs-intmath-patch
            (lambda _
              (for-each
               (lambda (f)
                 (substitute* f
                   (("#include \"libavutil/avassert.h\"" all)
                    (string-append all "\n#include \"libavutil/intmath.h\""))))
               '("libavcodec/cbs_av1.c" "libavcodec/cbs_h2645.c"))
              (substitute* "libavcodec/cbs_sei_syntax_template.c"
                (("SEI_FUNC\\(filler_payload" all)
                 (string-append "#include \"libavutil/intmath.h\"\n\n" all)))))
          ;; FFmpeg's configure is not autotools (chokes on CONFIG_SHELL=,
          ;; --build=, ...), so drive it explicitly with build-deps' minimal
          ;; static flag set.  There is no /bin/sh in the build container, so
          ;; rewrite the `#!/bin/sh' the sanity test writes into $TMPDIR (same
          ;; trick Guix's own ffmpeg package uses).
          (replace 'configure
            (lambda _
              (substitute* "configure"
                (("#! */bin/sh") (string-append "#!" (which "sh"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (invoke "./configure"
                      (string-append "--prefix=" #$output "/ffmpeg")
                      "--pkg-config-flags=--static"
                      "--extra-libs=-lpthread -lm"
                      "--enable-pic"
                      "--disable-all"
                      "--disable-autodetect"
                      "--disable-iconv"
                      "--enable-gpl"
                      "--enable-static"
                      "--enable-avcodec"
                      "--enable-avutil"
                      "--enable-bsfs"
                      "--enable-swscale"
                      "--enable-libsvtav1" "--enable-encoder=libsvtav1"
                      "--enable-libx264" "--enable-encoder=libx264"
                      "--enable-libx265" "--enable-encoder=libx265"
                      "--enable-vaapi"
                      "--enable-encoder=h264_vaapi,hevc_vaapi,av1_vaapi"
                      "--enable-vulkan"
                      "--enable-encoder=h264_vulkan,hevc_vulkan,av1_vulkan")))
          ;; Compile libcbs.a from the configured/built source tree.
          (add-after 'build 'build-cbs
            (lambda _
              (for-each
               (lambda (c)
                 (invoke "gcc" "-c" "-fPIC" "-I." c
                         "-o" (string-append c ".o")
                         "-Wno-incompatible-pointer-types"
                         "-Wno-format" "-Wno-format-extra-args"))
               '#$%cbs-source-files)
              (apply invoke "ar" "rcs" "libcbs.a"
                     (map (lambda (c) (string-append c ".o"))
                          '#$%cbs-source-files))))
          ;; After `make install' (libav*.a + public headers), add libcbs.a, the
          ;; internal cbs headers, and the static codec archives.
          (add-after 'install 'install-cbs-and-codecs
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((ff  (string-append #$output "/ffmpeg"))
                     (lib (string-append ff "/lib"))
                     (inc (string-append ff "/include")))
                (install-file "libcbs.a" lib)
                (install-file "config.h" inc)
                (for-each
                 (lambda (h)
                   (install-file (string-append "libavcodec/" h)
                                 (string-append inc "/libavcodec")))
                 '#$%cbs-avcodec-headers)
                (when (file-exists? "libavcodec/hevc/hevc.h")
                  (install-file "libavcodec/hevc/hevc.h"
                                (string-append inc "/libavcodec/hevc")))
                (for-each
                 (lambda (h)
                   (install-file (string-append "libavutil/" h)
                                 (string-append inc "/libavutil")))
                 '("attributes.h" "attributes_internal.h" "intmath.h"))
                (when (file-exists? "libavcodec/x86/mathops.h")
                  (install-file "libavcodec/x86/mathops.h"
                                (string-append inc "/libavcodec/x86")))
                (for-each
                 (lambda (h)
                   (when (file-exists? (string-append "libavutil/x86/" h))
                     (install-file (string-append "libavutil/x86/" h)
                                   (string-append inc "/libavutil/x86"))))
                 '("asm.h" "intmath.h"))))))))
    ;; Link x264/x265/SVT-AV1 dynamically from Guix (their static builds don't
    ;; export their public API cleanly).  libav*.a keeps undefined refs to these
    ;; that Sunshine resolves against the shared libs at its final link.
    (inputs
     (list libx264
           x265-4
           svt-av1-3
           libva
           vulkan-loader
           vulkan-headers))
    (native-inputs
     (list pkg-config nasm))
    (home-page "https://www.ffmpeg.org/")
    (synopsis "Static FFmpeg + libcbs for Sunshine, built entirely from Guix")
    (description "A static FFmpeg build (avcodec/avutil/swscale with x264, x265,
SVT-AV1, VAAPI and Vulkan encoders) plus the @code{libcbs} coded-bitstream
library that Sunshine links against.  This is the unvendored counterpart of the
prebuilt @code{sunshine-ffmpeg-prebuilt}, sourced entirely from Guix packages.")
    (license license:gpl2+)))

(define %sunshine-version "2026.516.143833")

;; Both public variants share everything except which package supplies the
;; FFMPEG_PREPARED_BINARIES tree.  `shared-codecs' is non-empty for the
;; unvendored build: those libs are linked dynamically and appended to
;; FFMPEG_LIBRARIES (see the link-shared-codecs phase).
(define* (make-sunshine ffmpeg-tree #:optional (shared-codecs '()))
  (package
    (name "sunshine")
    (version %sunshine-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LizardByte/Sunshine.git")
                    (commit (string-append "v" %sunshine-version))
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
          #$@(if (null? shared-codecs)
                 '()
                 (list
                  #~(add-after 'unpack 'link-shared-codecs
                      (lambda _
                        ;; The unvendored libav*.a carry undefined refs to the
                        ;; codecs; make Sunshine link Guix's shared x264/x265/
                        ;; SVT-AV1 by appending them to FFMPEG_LIBRARIES.
                        (substitute* "cmake/dependencies/ffmpeg.cmake"
                          (("set\\(FFMPEG_INCLUDE_DIRS \"\\$\\{FFMPEG_PREPARED_BINARIES\\}/include\"\\)" all)
                           (string-append
                            all
                            "\nlist(APPEND FFMPEG_LIBRARIES SvtAv1Enc x264 x265)")))))))
          (add-before 'configure 'set-version
            (lambda _
              (setenv "BRANCH" (string-append "v" #$%sunshine-version))
              (setenv "BUILD_VERSION" #$%sunshine-version)))
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
     (append
      shared-codecs
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
      avahi)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("tar" ,tar)
       ("zstd" ,zstd)
       ;; The FFmpeg tree (prebuilt or Guix-built) consumed via
       ;; -DFFMPEG_PREPARED_BINARIES.
       ("sunshine-ffmpeg" ,ffmpeg-tree)
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
                               %sunshine-version "/sunshine-" %sunshine-version
                               "-1-x86_64.pkg.tar.zst"))
           (file-name (string-append "sunshine-web-ui-" %sunshine-version ".pkg.tar.zst"))
           (sha256 (base32 "16ajzidb4z2pnjx5c2dybbr5pk9r5kbqa6q0www1zy3rn37kjrb7"))))))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description "Sunshine is a self-hosted game stream host for Moonlight. Offering low latency, cloud gaming server capabilities with support for AMD, Intel, and Nvidia GPUs for hardware encoding. Software encoding is also available.")
    (license license:gpl3)))

;; Vendored: link LizardByte's prebuilt static FFmpeg blob (self-contained).
(define-public sunshine/vendored
  (make-sunshine sunshine-ffmpeg))

;; Unvendored: FFmpeg built from Guix source, codecs linked from Guix shared libs.
(define-public sunshine/unvendored
  (make-sunshine sunshine-ffmpeg-guix (list libx264 x265-4 svt-av1-3)))

;; Default `sunshine' -> the vendored (prebuilt-FFmpeg) build.
(define-public sunshine sunshine/vendored)

sunshine
