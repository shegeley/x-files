(define-module (x-files packages mission-center)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages base) #:select (patch))
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages cmake) #:select (cmake-minimal))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages gl) #:select (mesa))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk))
  #:use-module ((gnu packages linux) #:select (eudev))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((gnu packages xdisorg) #:select (libdrm))
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Mission Center 1.2.0 targets the GNOME 50 stack (glib >= 2.88, gtk ~4.24);
;; Guix currently ships glib 2.86 / gtk 4.22 (GNOME 49), so 1.1.0 — which
;; targets GNOME 48 (gtk4 0.10 "gnome_48", libadwaita "v1_8") — is the newest
;; release that builds here.  Revisit once Guix's glib/gtk are bumped.
(define %mission-center-version "1.1.0")

;; The gatherer (magpie/platform-linux) compiles nvtop's GPU-info C sources.
;; Its build.rs would fetch this exact tarball over the network; we provide it
;; offline instead (see the MC_NVTOP_TARBALL patch below).
(define nvtop-source
  (origin
    (method url-fetch)
    (uri (string-append
          "https://github.com/Syllo/nvtop/archive/"
          "339ee0b10a64ec51f43d27357b0068a40f16e9e4.tar.gz"))
    (file-name "nvtop-339ee0b.tar.gz")
    (sha256
     (base32 "0m9yplcs7g5zz95p9q210jqi6jcb51m9hv41hmknfw81544rjkik"))))

(define mission-center-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/mission-center-devs/mission-center")
          (commit (string-append "v" %mission-center-version))
          ;; The `magpie' gatherer lives in the `subprojects/magpie' submodule.
          (recursive? #t)))
    (file-name (git-file-name "mission-center" %mission-center-version))
    (sha256
     (base32 "1pqr8rcbqvn9c537iadg100lgciw76fcq2yqvvhlpi4k685dli18"))))

;; Both the main app and the `magpie' subproject shell out to `cargo build',
;; and Guix builds run offline.  Vendor every crate from BOTH Cargo.lock files
;; (the app's and magpie's, via --sync) into one fixed-output derivation.  The
;; printed cargo config (which also pins the git-sourced `upower_dbus' crate)
;; is captured next to the vendored crates as "config".
(define mission-center-cargo-vendor
  (computed-file
   "mission-center-cargo-vendor"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append (ungexp rust "cargo") "/bin:"
                                (ungexp rust) "/bin:"
                                (ungexp git-minimal) "/bin"))
         (setenv "HOME" "/tmp")
         (setenv "CARGO_HOME" "/tmp/cargo-home")
         (mkdir-p "/tmp/cargo-home")
         ;; nss-certs ships only hashed per-CA files; cargo's libcurl wants a
         ;; single bundle.  Concatenate them and point curl at it.
         (let ((bundle "/tmp/ca-bundle.crt"))
           (call-with-output-file bundle
             (lambda (out)
               (for-each
                (lambda (cert)
                  (call-with-input-file cert
                    (lambda (in) (dump-port in out))))
                (find-files (string-append (ungexp nss-certs) "/etc/ssl/certs")
                            "\\.0$"))))
           (setenv "SSL_CERT_FILE" bundle)
           (setenv "CURL_CA_BUNDLE" bundle))
         (copy-recursively (ungexp mission-center-source) "source")
         (for-each make-file-writable (find-files "source"))
         (chdir "source")
         (mkdir-p (ungexp output))
         (invoke (string-append (ungexp bash-minimal) "/bin/bash") "-c"
                 (string-append
                  "cargo vendor --locked"
                  " --sync subprojects/magpie/Cargo.toml "
                  (ungexp output) "/vendor > "
                  (ungexp output) "/config"))
         ;; cargo prints the vendored directory as an absolute path into the
         ;; config.  That path is this FOD's own (hash-derived) output, so
         ;; embedding it verbatim makes the content hash self-referential and
         ;; unable to converge.  Replace it with a token the main build swaps
         ;; back for the real store path.
         (substitute* (string-append (ungexp output) "/config")
           (((string-append (ungexp output) "/vendor")) "__VENDOR_DIR__"))))
   #:options (list #:hash-algo 'sha256
                   #:hash (base32 "1a6nkxq5i5d0sq4lqvvw9yx4wh5pin4gbv2a4if76faa8ds77rn8")
                   #:recursive? #t)))

(define-public mission-center
  (package
    (name "mission-center")
    (version %mission-center-version)
    (source mission-center-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson tests are appstream/desktop/gschema validation only.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo-and-nvtop
            (lambda _
              ;; gcc-toolchain ships `gcc' but no `cc'; cc-rs (used by several
              ;; crates that compile bundled C) needs one.
              (setenv "CC" "gcc")
              ;; Both meson builds force CARGO_HOME to a build-tree path we
              ;; can't populate ahead of time.  Drop those overrides so cargo
              ;; honours the CARGO_HOME we export below (which holds the
              ;; vendored-sources config).
              (substitute* "src/meson.build"
                (("cargo_env \\+= \\[ 'CARGO_HOME=.*") ""))
              (substitute* "subprojects/magpie/meson.build"
                (("cargo_env \\+= \\['CARGO_HOME=.*") ""))
              (let ((cargo-home (string-append (getcwd) "/../cargo-home"))
                    (config (string-append (getcwd) "/../cargo-home/config.toml")))
                (mkdir-p cargo-home)
                (copy-file (string-append #$mission-center-cargo-vendor
                                          "/config")
                           config)
                (make-file-writable config)
                ;; Point the vendored-sources token at the real store path and
                ;; forbid any network access.
                (substitute* config
                  (("__VENDOR_DIR__")
                   (string-append #$mission-center-cargo-vendor "/vendor")))
                (let ((port (open-file config "a")))
                  (display "\n[net]\noffline = true\n" port)
                  (close-port port))
                (setenv "CARGO_HOME" cargo-home))
              ;; The gatherer's build.rs downloads nvtop over the network; make
              ;; it use the tarball we provide offline instead.
              (substitute* "subprojects/magpie/platform-linux/build.rs"
                (("    let mut response = ureq::get\\(url\\)\\.call\\(\\)\\?;")
                 (string-append
                  "    if let Ok(p) = std::env::var(\"MC_NVTOP_TARBALL\") "
                  "{ return Ok(p); }\n"
                  "    let mut response = ureq::get(url).call()?;")))
              (setenv "MC_NVTOP_TARBALL" #$nvtop-source)))
          (add-after 'install 'wrap-runtime
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                ;; The app spawns the `missioncenter-magpie' gatherer by bare
                ;; name, and the gatherer looks up its hardware-name database at
                ;; a fixed /usr path.  Point both at this package.
                (wrap-program (string-append bin "/missioncenter")
                  `("PATH" ":" prefix (,bin))
                  `("MC_MAGPIE_HW_DB" ":" =
                    (,(string-append out "/share/missioncenter/hw.db")))))))
          (add-after 'install 'delete-icon-cache
            (lambda* (#:key outputs #:allow-other-keys)
              ;; meson's post_install bakes a per-package icon-theme.cache with
              ;; a 1970 mtime, which stops GTK from scanning the directory and
              ;; blocks the profile hook from merging a correct cache.  Drop it.
              (let ((cache (string-append (assoc-ref outputs "out")
                                          "/share/icons/hicolor/icon-theme.cache")))
                (when (file-exists? cache)
                  (delete-file cache))))))))
    (native-inputs
     (list blueprint-compiler
           cmake-minimal
           desktop-file-utils
           ;; Several crates compile bundled C; provide a cc for cc-rs.
           gcc-toolchain
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")
           ;; magpie/platform-linux's build.rs applies patches to nvtop.
           patch
           pkg-config
           ;; data/hwdb generates hw.db with a Python script.
           python
           rust
           `(,rust "cargo")))
    (inputs
     (list ;; Used by wrap-program for the `missioncenter' wrapper.
           bash-minimal
           eudev                        ;libudev
           glib
           gtk
           libadwaita
           libdrm
           mesa))                        ;gbm, libEGL
    (home-page "https://missioncenter.io")
    (synopsis "Monitor system resources: CPU, memory, disk, network and GPU")
    (description
     "Mission Center is a GTK4/libadwaita system monitor.  It shows real-time
usage of the CPU (overall and per core), memory, disks, network interfaces and
GPUs, and lists running applications and processes with their resource use.
System data is collected by a separate @code{missioncenter-magpie} gatherer
process.")
    (license license:gpl3+)))
