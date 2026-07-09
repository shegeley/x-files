(define-module (x-files packages rutracker-cli)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages base) #:select (tar))
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module ((gnu packages freedesktop) #:select (xdg-utils))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages rust) #:select (rust))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((gnu packages xdisorg) #:select (wl-clipboard xclip xsel))
  #:use-module ((guix build-system cargo) #:select (crate-uri))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (rutracker-cli))

(define %rutracker-cli-version "0.1.0")

;; A published crates.io tarball, not a git checkout: the crate ships its own
;; Cargo.lock, which `cargo vendor --locked' below pins against.
(define rutracker-cli-source
  (origin
    (method url-fetch)
    (uri (crate-uri "rutracker-cli" %rutracker-cli-version))
    (file-name (string-append "rutracker-cli-" %rutracker-cli-version ".tar.gz"))
    (sha256
     (base32 "0l04apf2h8c2y73b6bbpcllqvp6697c5xgaiifn7a4x4d0vwk0sb"))))

;; rutracker-cli is a plain Cargo project (no meson wrapper), so we drive
;; `cargo build' by hand over the gnu-build-system.  Guix builds run offline;
;; pre-fetch every crate from Cargo.lock into a vendored directory in a
;; fixed-output derivation (network allowed) whose hash pins the exact set of
;; sources.  The main build then consumes it fully offline.  Same recipe as
;; ntfyr, minus the meson bits.
(define rutracker-cli-cargo-vendor
  (computed-file
   "rutracker-cli-cargo-vendor"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (setenv "PATH"
                 (string-append (ungexp rust "cargo") "/bin:"
                                (ungexp rust) "/bin:"
                                (ungexp git-minimal) "/bin:"
                                (ungexp tar) "/bin:"
                                (ungexp gzip) "/bin"))
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
         ;; The crate is a tarball (not a git checkout); unpack it and vendor
         ;; from the extracted tree.
         (invoke "tar" "--extract" "--file" (ungexp rutracker-cli-source))
         (chdir (string-append "rutracker-cli-" (ungexp %rutracker-cli-version)))
         (invoke "cargo" "vendor" "--locked" (ungexp output))))
   #:options (list #:hash-algo 'sha256
                   #:hash (base32 "1ai5s0dyclhf1y27mr4xqc39vb80pc62wnm5p6a6gm14yn0w1kn0")
                   #:recursive? #t)))

(define-public rutracker-cli
  (package
    (name "rutracker-cli")
    (version %rutracker-cli-version)
    (source rutracker-cli-source)
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The test suite needs network access to rutracker.org.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'bootstrap)
          (replace 'configure
            (lambda _
              ;; gcc-toolchain ships `gcc' but no `cc'; point cc-rs (used by the
              ;; `ring' crate) at it so it can compile its bundled C.
              (setenv "CC" "gcc")
              (setenv "CARGO_HOME" (string-append (getcwd) "/.cargo-home"))
              ;; Redirect crates.io to the vendored sources and force cargo
              ;; fully offline.
              (mkdir-p ".cargo")
              (call-with-output-file ".cargo/config.toml"
                (lambda (port)
                  (format port "\
[source.crates-io]
replace-with = \"vendored-sources\"

[source.vendored-sources]
directory = \"~a\"

[net]
offline = true
"
                          #$rutracker-cli-cargo-vendor)))))
          (replace 'build
            (lambda _
              (invoke "cargo" "build" "--release" "--frozen")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/rutracker-cli"))
                     (bash-comp (string-append
                                 out "/share/bash-completion/completions"))
                     (zsh-comp (string-append out "/share/zsh/site-functions"))
                     (fish-comp (string-append
                                 out "/share/fish/vendor_completions.d")))
                (install-file "target/release/rutracker-cli"
                              (string-append out "/bin"))
                (mkdir-p bash-comp)
                (mkdir-p zsh-comp)
                (mkdir-p fish-comp)
                (invoke "sh" "-c"
                        (string-append bin " completions bash > "
                                       bash-comp "/rutracker-cli"))
                (invoke "sh" "-c"
                        (string-append bin " completions zsh > "
                                       zsh-comp "/_rutracker-cli"))
                (invoke "sh" "-c"
                        (string-append bin " completions fish > "
                                       fish-comp "/rutracker-cli.fish")))))
          (add-after 'install 'wrap-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                ;; Clipboard copy (magnet links) and opening URLs shell out to
                ;; these tools at runtime; keep them on PATH.
                (wrap-program (string-append out "/bin/rutracker-cli")
                  `("PATH" prefix
                    (,(string-append (assoc-ref inputs "xdg-utils") "/bin")
                     ,(string-append (assoc-ref inputs "wl-clipboard") "/bin")
                     ,(string-append (assoc-ref inputs "xclip") "/bin")
                     ,(string-append (assoc-ref inputs "xsel") "/bin"))))))))))
    (native-inputs
     (list gcc-toolchain
           rust
           `(,rust "cargo")))
    (inputs
     (list bash-minimal xdg-utils wl-clipboard xclip xsel))
    (home-page "https://github.com/johnlepikhin/rutracker-cli")
    (synopsis "Command-line, TUI and MCP client for rutracker.org")
    (description
     "@command{rutracker-cli} is a Rust client for rutracker.org with three
interfaces: a traditional command-line tool, an interactive terminal UI built
with @code{ratatui}, and a Model Context Protocol server for use by AI
assistants.  It supports searching, downloading torrent files, copying magnet
links, and triggering an external torrent client (e.g.@: a remote Transmission
daemon via RPC).  Configuration follows XDG conventions at
@file{$XDG_CONFIG_HOME/rutracker-cli/config.yaml}.")
    (license (list license:expat license:asl2.0))))
