(define-module (x-files packages keypunch)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Keypunch has no release tags in its git history at the commit we pin to;
;; the version below is the project_version set in meson.build.
(define %keypunch-version "7.0")
(define %keypunch-commit "ad2812dc0f8471ac57762704dad16ccb5ddf60fc")

(define keypunch-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/bragefuglseth/keypunch")
          (commit %keypunch-commit)))
    (file-name (git-file-name "keypunch" %keypunch-version))
    (sha256
     (base32 "070bbrv0hfwzyjc8fwr72rlibkmp6kjvvgllzs9hpykv4rv0crlm"))))

;; The build shells out to `cargo build', and Guix builds run offline.
;; Vendor every crate from Cargo.lock into one fixed-output derivation, the
;; same way (x-files packages mission-center) does for Mission Center.
;; Unlike Mission Center, Keypunch has no subprojects/submodules, so a plain
;; `cargo vendor --locked' against its own Cargo.lock is enough.
(define keypunch-cargo-vendor
  (computed-file
   "keypunch-cargo-vendor"
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
         (copy-recursively (ungexp keypunch-source) "source")
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
                   #:hash (base32 "1mh2j2jqywnv9zxpk0jq9m0vjpc5609l5ww9chfxblgf7a2cfnlp")
                   #:recursive? #t)))

(define-public keypunch
  (package
    (name "keypunch")
    (version %keypunch-version)
    (source keypunch-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; Meson's default build type is "debugoptimized"; Keypunch's
      ;; meson.build appends ".Devel" to the app ID for any build type other
      ;; than "release", which would otherwise ship a devel .desktop/icon.
      #:build-type "release"
      ;; The meson tests are appstream/desktop/gschema validation only.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo
            (lambda _
              ;; src/meson.build hard-codes CARGO_HOME to a build-tree path
              ;; we can't populate ahead of time; drop that override so
              ;; cargo honours the CARGO_HOME we export below (which holds
              ;; the vendored-sources config).
              (substitute* "src/meson.build"
                (("cargo_env = \\[ 'CARGO_HOME=' \\+ meson\\.project_build_root\\(\\) / 'cargo-home' \\]")
                 "cargo_env = []")
                (("cargo_env = \\[ 'CARGO_HOME=' \\+ meson\\.project_source_root\\(\\) / 'cargo' \\]")
                 "cargo_env = []"))
              (let ((cargo-home (string-append (getcwd) "/../cargo-home"))
                    (config (string-append (getcwd) "/../cargo-home/config.toml")))
                (mkdir-p cargo-home)
                (copy-file (string-append #$keypunch-cargo-vendor "/config")
                           config)
                (make-file-writable config)
                ;; Point the vendored-sources token at the real store path
                ;; and forbid any network access.
                (substitute* config
                  (("__VENDOR_DIR__")
                   (string-append #$keypunch-cargo-vendor "/vendor")))
                (let ((port (open-file config "a")))
                  (display "\n[net]\noffline = true\n" port)
                  (close-port port))
                (setenv "CARGO_HOME" cargo-home)))))))
    (native-inputs
     (list blueprint-compiler
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")
           pkg-config
           rust
           `(,rust "cargo")))
    (inputs
     (list glib
           gtk
           libadwaita))
    (home-page "https://github.com/bragefuglseth/keypunch")
    (synopsis "Practice your typing skills")
    (description
     "Keypunch is a typing test application for GNOME.  It measures typing
speed and accuracy across a variety of included texts, supports custom text
snippets, and offers multiple languages and difficulty levels.")
    (license license:gpl3+)))
