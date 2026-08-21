(define-module (x-files packages railway)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages freedesktop) #:select (appstream desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Railway (GNOME app-id de.schmidhuberj.DieBahn, marketed on GNOME's app
;; store simply as "DieBahn") is a GTK4/libadwaita train-journey planner.
;; Like Mission Center, it is a meson project whose actual build step shells
;; out to `cargo build'; Guix builds run offline, so every crate from
;; Cargo.lock is vendored ahead of time into a fixed-output derivation.
(define %railway-version "2.10.1")

(define railway-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/schmiddi-on-mobile/railway")
          (commit "28d0aaabb30cda6bafbffdd0d6bbd99bc7e51a0c")))
    (file-name (git-file-name "railway" %railway-version))
    (sha256
     (base32 "1dsdk37c2zxhg954yfmp6ljswivs64agpmz51n6wj2g8m7y2lq8s"))))

;; The meson build shells out to `cargo build' and Guix builds are offline,
;; so vendor every crate from Cargo.lock into one fixed-output derivation
;; (all 282 entries are plain crates.io sources; no git dependencies).
(define railway-cargo-vendor
  (computed-file
   "railway-cargo-vendor"
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
         (copy-recursively (ungexp railway-source) "source")
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
                   #:hash (base32 "1py3zwnqq718fncqvivrcdxcj68mmfza7a60phhhlpslakm4qwv7")
                   #:recursive? #t)))

(define-public railway
  (package
    (name "railway")
    (version %railway-version)
    (source railway-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson tests are appstream/desktop/gschema validation plus
      ;; `cargo test', which would need network access; skip them.
      #:tests? #f
      #:configure-flags #~(list "-Dprofile=default")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'configure-offline-cargo
            (lambda _
              ;; openssl-sys's build.rs runs a C preprocessor (via bindgen)
              ;; to probe the OpenSSL headers; gcc-toolchain ships `gcc' but
              ;; no `cc', so point it there explicitly.
              (setenv "CC" "gcc")
              ;; The meson build forces CARGO_HOME to a build-tree path we
              ;; can't populate ahead of time.  Drop that override so cargo
              ;; honours the CARGO_HOME we export below (which holds the
              ;; vendored-sources config).
              (substitute* "src/meson.build"
                (("cargo_env = \\[ 'CARGO_HOME=.*") "cargo_env = []\n"))
              (let ((cargo-home (string-append (getcwd) "/../cargo-home"))
                    (config (string-append (getcwd) "/../cargo-home/config.toml")))
                (mkdir-p cargo-home)
                (copy-file (string-append #$railway-cargo-vendor "/config")
                           config)
                (make-file-writable config)
                ;; Point the vendored-sources token at the real store path and
                ;; forbid any network access.
                (substitute* config
                  (("__VENDOR_DIR__")
                   (string-append #$railway-cargo-vendor "/vendor")))
                (let ((port (open-file config "a")))
                  (display "\n[net]\noffline = true\n" port)
                  (close-port port))
                (setenv "CARGO_HOME" cargo-home)))))))
    (native-inputs
     (list appstream
           blueprint-compiler
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
           gtk
           libadwaita
           openssl))
    (home-page "https://mobile.schmidhuberj.de/railway")
    (synopsis "Find train journeys and travel information")
    (description
     "Railway (GNOME application ID @code{de.schmidhuberj.DieBahn}, listed on
GNOME's app store as @acronym{DieBahn}) is a GTK4/libadwaita application for
searching train journeys, comparing routes and departure times, and tracking
real-time delays and platform changes across a range of transit providers.")
    (license license:gpl3+)))
