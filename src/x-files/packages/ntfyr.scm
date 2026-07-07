(define-module (x-files packages ntfyr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages freedesktop) #:select (appstream
                                                     desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler
                                               libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk gtksourceview))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages sqlite) #:select (sqlite))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define %ntfyr-version "0.7.1")

(define ntfyr-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/tobagin/Ntfyr")
          (commit (string-append "v" %ntfyr-version))))
    (file-name (git-file-name "ntfyr" %ntfyr-version))
    (sha256
     (base32 "0z97pbaran8b4cz8d777xkbpk0paaa0142my56g48rpf6ab5skks"))))

;; Ntfyr's meson build shells out to "cargo build".  Guix builds run offline,
;; so pre-fetch every crate from Cargo.lock into a vendored directory.  This is
;; a fixed-output derivation (network is allowed) whose hash pins the exact set
;; of vendored sources; the main build then consumes it fully offline.
(define ntfyr-cargo-vendor
  (computed-file
   "ntfyr-cargo-vendor"
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
         ;; single bundle.  Concatenate them into one and point curl at it.
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
         (copy-recursively (ungexp ntfyr-source) "source")
         (for-each make-file-writable (find-files "source"))
         (chdir "source")
         (invoke "cargo" "vendor" "--locked" (ungexp output))))
   #:options (list #:hash-algo 'sha256
                   #:hash (base32 "1nm2a2y12yg2xdirf2asb8dssjzhlvl9s3wr4li8wfz59prpx0d5")
                   #:recursive? #t)))

(define-public ntfyr
  (package
    (name "ntfyr")
    (version %ntfyr-version)
    (source ntfyr-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson test suite is pure appstream/desktop/gschema validation,
      ;; which is fragile and adds nothing to the actual binary build.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'configure-cargo-vendor
            (lambda _
              ;; gcc-toolchain ships `gcc' but no `cc'; point cc-rs (used by the
              ;; `ring' crate) at it so it can compile its bundled C.
              (setenv "CC" "gcc")
              ;; src/meson.build sets CARGO_HOME to <source>/cargo when that
              ;; directory exists.  Drop a config there that redirects crates.io
              ;; to the vendored sources and forces cargo fully offline.
              (mkdir-p "cargo")
              (call-with-output-file "cargo/config.toml"
                (lambda (port)
                  (format port "\
[source.crates-io]
replace-with = \"vendored-sources\"

[source.vendored-sources]
directory = \"~a\"

[net]
offline = true
"
                          #$ntfyr-cargo-vendor))))))))
    (native-inputs
     (list appstream
           blueprint-compiler
           desktop-file-utils
           ;; The `ring' crate compiles C; provide a cc for cc-rs.
           gcc-toolchain
           gettext-minimal
           `(,glib "bin")
           ;; Provides gtk4-update-icon-cache, required by meson's post_install.
           `(,gtk "bin")
           pkg-config
           rust
           `(,rust "cargo")))
    (inputs
     (list glib
           gtk
           gtksourceview
           libadwaita
           ;; oo7's file/secret backend links the system sqlite.
           sqlite))
    (home-page "https://github.com/tobagin/Ntfyr")
    (synopsis "GTK4 desktop client for ntfy push notifications")
    (description
     "Ntfyr is a native GTK4/libadwaita desktop application for subscribing to
@url{https://ntfy.sh,ntfy} topics and receiving push notifications.  It
supports multiple servers and accounts, access-token authentication, message
filtering, a system tray icon, and Markdown-rendered notification bodies.")
    (license license:gpl3+)))
