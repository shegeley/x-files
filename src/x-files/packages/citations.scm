(define-module (x-files packages citations)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages cmake) #:select (cmake-minimal))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk gtksourceview))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages pdf) #:select (poppler))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; ----------------------------------------------------------------------
;; STATUS: does NOT build yet.  Left here (uncommitted) as a real, mostly-
;; working packaging attempt and a precise diagnosis of the actual blocker,
;; for whoever revisits this once Guix ships a newer gtk4.
;;
;; Citations at this commit is developed against a rolling GNOME "World"
;; stack ahead of any stable GTK4 release:
;;
;;  - meson.build hard-requires gtk4 >= 4.23 (an unstable/devel version
;;    number; there is no GTK 4.23 or 4.24 stable release at all as of this
;;    writing).  Worked around below in 'relax-gtk4-version-requirement.
;;
;;  - The workspace Cargo.toml enables gtk4-rs's "gnome_47" / "v4_24"
;;    features, which compile in bindings for GTK API that plain doesn't
;;    exist in Guix's gtk4 4.22.1 -- confirmed with
;;    `nm -D --defined-only .../libgtk-4.so | grep enum_list`, which finds
;;    nothing.  Downgrading that to the "gnome_49" tier (matching what Guix
;;    actually ships: glib 2.86 / gtk4 4.22, same as the GNOME-49 note in
;;    (x-files packages mission-center)) gets the vendored gtk4-rs crates
;;    themselves to compile fine -- see 'downgrade-gtk4-feature-tier.
;;
;;  - But Citations' OWN application code, not just crate feature bloat,
;;    directly names `gtk::EnumListItem' (a type that only exists with the
;;    v4_24 feature enabled) in four places: src/citation_preview.rs:265,
;;    src/citation_preview.rs:275, src/entry_page.rs:199, and
;;    src/new_entry_dialog.rs:79 -- all BibTeX entry-type dropdown widgets.
;;    That is an irreducible, real dependency on unreleased GTK4 API, not a
;;    packaging artifact; there is no version-gate left to relax.  Building
;;    this package needs either GTK4 >= 4.24 (once it exists and Guix
;;    packages it) or an upstream patch to Citations itself, neither of
;;    which belongs in a package definition.
;; ----------------------------------------------------------------------
(define %citations-version "0.6.2")
(define %citations-commit "10156b26e6275b3d7d312943759b23a375a9ae31")

(define citations-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.gnome.org/World/citations")
          (commit %citations-commit)))
    (file-name (git-file-name "citations" %citations-version))
    (sha256
     (base32 "16wy72day61fm9imv94f5m18ja6nfd0pgl4fwvr92ypzd9dlnjf0"))))

;; The meson build shells out to `cargo build', and Guix builds run offline.
;; Vendor every crate from the workspace's single Cargo.lock (citations +
;; the cratebibtex/texer workspace members, all path deps, no git deps) into
;; one fixed-output derivation, exactly as for mission-center.
(define citations-cargo-vendor
  (computed-file
   "citations-cargo-vendor"
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
         (copy-recursively (ungexp citations-source) "source")
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
                   #:hash (base32 "0nkfbw27cgr3w16g71mb0qijfa5k45qyqn8faqspcf5nwwxa548g")
                   #:recursive? #t)))

(define-public citations
  (package
    (name "citations")
    (version %citations-version)
    (source citations-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson tests are cargo-test/cargo-clippy plus appstream/desktop
      ;; validation; none of that is meaningful in an offline build sandbox.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-gtk4-version-requirement
            (lambda _
              ;; See the comment above %citations-version: Guix ships gtk4
              ;; 4.22.1, older than upstream's >= 4.23 courtesy bound.
              (substitute* "meson.build"
                (("dependency\\('gtk4', version: '>= 4.23'\\)")
                 "dependency('gtk4', version: '>= 4.22')"))))
          (add-after 'unpack 'downgrade-gtk4-feature-tier
            (lambda _
              ;; The actual cause of the missing symbols (see above): drop
              ;; the "v4_24" Cargo feature (GTK API newer than any stable
              ;; release) down to the "gnome_49" tier, which resolves to
              ;; gtk4-rs's "v4_20" feature bucket -- comfortably within what
              ;; Guix's gtk4 4.22.1 / glib 2.86.0 actually provide.
              (substitute* "Cargo.toml"
                (("features = \\[\"gnome_47\", \"v4_24\"\\]")
                 "features = [\"gnome_49\"]"))))
          (add-after 'unpack 'prepare-cargo
            (lambda _
              ;; gcc-toolchain ships `gcc' but no `cc'; cc-rs (used by
              ;; aws-lc-sys, among others) needs one.
              (setenv "CC" "gcc")
              ;; The top-level meson.build points CARGO_HOME at a path
              ;; under meson's build root (which doesn't exist until
              ;; 'configure runs, so we can't pre-populate it) and passes
              ;; that environment explicitly to the cargo custom_target,
              ;; overriding whatever we `setenv' here.  Repoint it at a
              ;; fixed path under the source root instead, which we CAN
              ;; populate now with the vendored-sources config.
              (let* ((cargo-home (string-append (getcwd) "/cargo-home"))
                     (config (string-append cargo-home "/config.toml")))
                (substitute* "meson.build"
                  (("cargo_home = meson\\.project_build_root\\(\\) / 'cargo-home'")
                   (string-append "cargo_home = '" cargo-home "'")))
                (mkdir-p cargo-home)
                (copy-file (string-append #$citations-cargo-vendor
                                          "/config")
                           config)
                (make-file-writable config)
                ;; Point the vendored-sources token at the real store path
                ;; and forbid any network access.
                (substitute* config
                  (("__VENDOR_DIR__")
                   (string-append #$citations-cargo-vendor "/vendor")))
                (let ((port (open-file config "a")))
                  (display "\n[net]\noffline = true\n" port)
                  (close-port port)))))
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
     (list cmake-minimal
           desktop-file-utils
           ;; aws-lc-sys (pulled in transitively via reqwest/rustls) compiles
           ;; bundled C; provide a cc for cc-rs.
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
           gtksourceview
           libadwaita
           poppler))
    (home-page "https://gitlab.gnome.org/World/citations")
    (synopsis "Bibliography and reference manager for GNOME")
    (description
     "Citations is a GTK4/libadwaita application to manage bibliography
databases in BibTeX format.  It can add entries manually, import them from a
DOI, and export a BibTeX file for use with LaTeX documents.")
    (license license:gpl3+)))
