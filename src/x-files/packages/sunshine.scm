(define-module (x-files packages streaming)
  ;; stolen https://github.com/forgoty/dotfiles/blob/master/guix/forgoty/packages/streaming.scm
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system node)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages node)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary))

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
         "-DNPM_OFFLINE=ON"
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
          (add-before 'configure 'set-version
            (lambda _
              (setenv "BRANCH" (string-append "v" #$version))
              (setenv "BUILD_VERSION" #$version)))
          (add-before 'build 'unpack-npm-cache
            (lambda* (#:key inputs #:allow-other-keys)
              (invoke "tar" "xzf" (assoc-ref inputs "npm-offline-cache"))
              (copy-file "package-lock.json" "../source/package-lock.json")
              (setenv "NPM_CONFIG_CACHE" (string-append (getcwd) "/offline-cache"))
              (with-directory-excursion "../source"
                (invoke "npm" "ci" "--offline")
                (for-each (lambda (bin)
                            (patch-shebang (string-append "node_modules/.bin/" (readlink bin))))
                            (find-files "node_modules/.bin" ".*"))))))))
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
      node
      nlohmann-json
      mesa
      avahi))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("tar" ,tar)
       ("npm-offline-cache" ,(origin
                              (method url-fetch)
                              (uri (string-append "https://github.com/forgoty/sunshine-web-ui-builder/releases/download/v" version "/npm-offline-cache.tar.gz"))
                              (file-name "npm-offline-cache.tar.gz")
                              (sha256 (base32 "18d6166vkp1xxq1r3mbvr5sjlqvsj5gsi8h8za6rw5c208j0p1pc"))))))
    (home-page "https://app.lizardbyte.dev/Sunshine/")
    (synopsis "Self-hosted game stream host for Moonlight")
    (description "Sunshine is a self-hosted game stream host for Moonlight. Offering low latency, cloud gaming server capabilities with support for AMD, Intel, and Nvidia GPUs for hardware encoding. Software encoding is also available.")
    (license license:gpl3)))

sunshine
