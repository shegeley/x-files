(define-module (x-files packages newsflash)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (glib-networking librsvg libadwaita))
  #:use-module ((gnu packages gstreamer) #:select (gstreamer
                                                     gst-plugins-base
                                                     gst-plugins-good
                                                     gst-plugins-bad))
  #:use-module ((gnu packages gtk) #:select (gtk))
  #:use-module ((gnu packages llvm) #:select (clang))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages sqlite) #:select (sqlite))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((gnu packages webkit) #:select (webkitgtk))
  #:use-module ((gnu packages xml) #:select (libxml2))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; NewsFlash 5.2.5 unconditionally depends (Cargo.toml, no feature gate) on
;; the `clapper-player' / `clapper-player-gtk' crates, whose `-sys' halves
;; probe pkg-config for `clapper-0.0' (>= 0.10) -- the C library from
;; https://github.com/Rafostar/clapper, a whole separate GStreamer-based
;; media-player project (its own meson build, GObject-Introspection, GTK4
;; widgets, optional EGL/DRM video sinks) that is packaged from source even
;; in NewsFlash's own Flatpak manifest (build-aux/clapper.json), alongside
;; libmicrodns (build-aux/libmicrodns.json, also unpackaged in Guix).  Guix
;; has neither `clapper' nor `libmicrodns'.  Packaging `clapper' (+ its own
;; `libmicrodns' dependency) is its own multi-file undertaking and out of
;; scope here; do that as a separate ticket, then revisit this package.
;;
;; An actual `guix build' of this package (source + cargo-vendor FOD both
;; verified good) currently fails *before* even reaching that clapper probe,
;; on a more fundamental blocker: Guix's `rust' (1.93.0, the newest in
;; mainline Guix as of this writing) is older than the MSRV some vendored
;; crates in Cargo.lock require, e.g.:
;;
;;   error: rustc 1.93.0 is not supported by the following package:
;;     kstring@2.0.4 requires rustc 1.96.0
;;
;; That would need either a newer `rust' in Guix, or selectively downgrading
;; individual crates in the vendor step (`cargo update -p kstring --precise
;; <ver>' etc, dropping `--locked') -- itself unverified to be a short list
;; once the first offender is patched around.  Fix the clapper gap and the
;; rustc-MSRV gap (both real, both confirmed by build attempts) before this
;; package will actually build.
(define %newsflash-version "5.2.5")
(define %newsflash-commit "72940ef939e1174e404d4e40e0c27e2a1c46818b")

(define newsflash-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/news-flash/news_flash_gtk")
          (commit %newsflash-commit)))
    (file-name (git-file-name "newsflash" %newsflash-version))
    (sha256
     (base32 "0mlmd8zarvwpz1r840njnvp2shlkmwxm350nipl9qxazg60h19k4"))))

;; The meson build shells out to `cargo build', and Guix builds run offline.
;; Vendor every crate from the single top-level Cargo.lock -- including the
;; two git dependencies (`news-flash' and `html2gtk', both pinned to a
;; commit on gitlab.com/news-flash) -- into one fixed-output derivation,
;; exactly as for mission-center/citations.  `cargo vendor' vendors
;; git-sourced crates too, so no extra `--sync' is needed here (unlike
;; mission-center's workspace-plus-subproject case).
(define newsflash-cargo-vendor
  (computed-file
   "newsflash-cargo-vendor"
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
         ;; single bundle.  Concatenate them and point curl at it (needed to
         ;; clone the two git dependencies over https).
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
         (copy-recursively (ungexp newsflash-source) "source")
         (for-each make-file-writable (find-files "source"))
         (chdir "source")
         (mkdir-p (ungexp output))
         (invoke (string-append (ungexp bash-minimal) "/bin/bash") "-c"
                 (string-append
                  "cargo vendor --locked "
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
                   #:hash (base32 "05qw8machiw3q6vi9bb60vzs4rhqipb7qhixazglxgwydgqmlmcb")
                   #:recursive? #t)))

(define-public newsflash
  (package
    (name "newsflash")
    (version %newsflash-version)
    (source newsflash-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson tests are appstream/desktop validation only; no cargo
      ;; test/clippy is wired into meson at all.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo
            (lambda _
              ;; gcc-toolchain ships `gcc' but no `cc'; cc-rs (used by
              ;; several crates that compile bundled C, e.g. openssl-sys)
              ;; needs one.
              (setenv "CC" "gcc")
              ;; `libxml' vendors bindgen with the "runtime" feature: it
              ;; dlopen(3)s libclang.so at build time (not link time) to
              ;; regenerate libxml2 bindings; point it at Guix's clang.
              (setenv "LIBCLANG_PATH"
                      (string-append #$clang "/lib"))
              ;; Unlike mission-center/citations, src/meson.build sets
              ;; CARGO_HOME unconditionally (a plain assignment, not an
              ;; appended flag) to a build-tree path we can't populate
              ;; ahead of time.  Blank it out so cargo honours the
              ;; CARGO_HOME we export below (which holds the
              ;; vendored-sources config).
              (substitute* "src/meson.build"
                (("cargo_env = \\[ 'CARGO_HOME=.*")
                 "cargo_env = []"))
              (let ((cargo-home (string-append (getcwd) "/../cargo-home"))
                    (config (string-append (getcwd) "/../cargo-home/config.toml")))
                (mkdir-p cargo-home)
                (copy-file (string-append #$newsflash-cargo-vendor
                                          "/config")
                           config)
                (make-file-writable config)
                ;; Point the vendored-sources token at the real store path
                ;; and forbid any network access.
                (substitute* config
                  (("__VENDOR_DIR__")
                   (string-append #$newsflash-cargo-vendor "/vendor")))
                (let ((port (open-file config "a")))
                  (display "\n[net]\noffline = true\n" port)
                  (close-port port))
                (setenv "CARGO_HOME" cargo-home))))
          (add-after 'install 'delete-icon-cache
            (lambda* (#:key outputs #:allow-other-keys)
              ;; meson's post_install bakes a per-package icon-theme.cache
              ;; with a 1970 mtime, which stops GTK from scanning the
              ;; directory and blocks the profile hook from merging a
              ;; correct cache.  Drop it.
              (let ((cache (string-append (assoc-ref outputs "out")
                                          "/share/icons/hicolor/icon-theme.cache")))
                (when (file-exists? cache)
                  (delete-file cache))))))))
    (native-inputs
     (list clang                       ;libclang.so for libxml's bindgen
           desktop-file-utils
           gcc-toolchain
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")
           pkg-config
           rust
           `(,rust "cargo")))
    (inputs
     (list glib
           glib-networking             ;runtime TLS support for GIO-based networking
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gst-plugins-bad
           gtk
           libadwaita
           librsvg                     ;gdk-pixbuf SVG loader, for the app icon
           libxml2
           openssl
           sqlite
           webkitgtk))
    (home-page "https://gitlab.com/news-flash/news_flash_gtk")
    (synopsis "GTK4 front-end for the NewsFlash feed-sync library")
    (description
     "NewsFlash is a GTK4/libadwaita RSS and Atom feed reader that
complements an existing web-based feed-sync account (Miniflux, Fever,
Feedbin, NewsBlur, Nextcloud News, and others).  It adds desktop
notifications, offline article caching, fast search and filtering, tagging,
and keyboard shortcuts on top of whatever service keeps feeds in sync across
devices.")
    (license license:gpl3+)))
