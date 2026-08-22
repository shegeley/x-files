(define-module (x-files packages yaak)
  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)               #:select (package
                                                        origin
                                                        base32
                                                        this-package-input))
  #:use-module ((gnu packages)                #:select (specification->package))
  #:use-module ((gnu packages node)           #:select (node))
  #:use-module ((gnu packages bash)           #:select (bash-minimal))
  #:use-module ((guix download)               #:select (url-fetch))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:use-module (guix gexp))

;; Yaak (yaak.app) is a Tauri (Rust + WebKitGTK) API client, shipped
;; upstream only as prebuilt release artifacts -- no npm/cargo source build
;; is attempted here.  nonguix's binary-build-system natively understands
;; .deb sources (its `binary-unpack' phase runs `ar x' + `tar xf
;; data.tar.*' on its own), so unlike (x-files packages spotify)/slojka's
;; snap/AppImage squashfs surgery, no custom unpack phase is needed here.
;;
;; The .deb installs a plain FHS tree: usr/bin/yaak-app-client plus
;; usr/lib/yaak/vendored/{node,protoc,plugins,plugin-runtime}/... (a
;; bundled Node runtime and a static protoc that Yaak shells out to, and
;; the JS plugin bundles it runs in-process).  Tauri resolves its resource
;; directory at runtime as "<exe-dir>/../lib/yaak" -- relative to the
;; running binary, not a baked-in /usr absolute path (confirmed: no "/usr"
;; string occurs in the binary).  So installing "usr/bin" -> "bin" and
;; "usr/lib" -> "lib" as siblings under $output preserves that relative
;; layout unmodified; no LD_LIBRARY_PATH wrapper script is needed, just
;; patchelf'd RPATHs.
(define %yaak-libs
  '("webkitgtk-for-gtk3" "gtk+" "cairo" "gdk-pixbuf" "glib" "libsoup"
    "dbus" "fontconfig" "zlib" "gcc-toolchain"))

(define-public yaak
  (package
    (name "yaak")
    (version "2026.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mountain-loop/yaak/releases/download/v"
                           version "/yaak_" version "_amd64.deb"))
       (sha256
        (base32 "1hdab23awgpjnbddbh80cn40mhvpsxnzf9g6640qcx2gzqxlfw4z"))))
    (build-system binary-build-system)
    (arguments
     (list
      ;; The prebuilt vendored/node/yaaknode binary (a ~120MB Node build
      ;; with an unusually large section count -- `file' reports "too many
      ;; notes") comes out ELIBBAD ("accessing a corrupted shared library")
      ;; after ANY `patchelf --set-interpreter' pass on it, confirmed with
      ;; both patchelf 0.16.1 (what nonguix pins) and 0.18.0 -- a genuine
      ;; patchelf layout bug on this binary, not fixed by version or by
      ;; `--output'ing to a fresh file.  Rather than patching a binary
      ;; patchelf cannot safely rewrite, 'discard-broken-vendored-node
      ;; below replaces it outright with a wrapper around this package's
      ;; own `node' input; it is invoked as a plain `<binary> <script.cjs>'
      ;; JS host with no special runtime requirements, so a stock Node
      ;; works as a drop-in.  #:validate-runpath? is left off since it
      ;; already errored (independently, on this same binary) before
      ;; 'discard-broken-vendored-node ever got a chance to run.
      #:validate-runpath? #f
      #:install-plan #~'(("usr/bin"   "bin")
                         ("usr/lib"   "lib")
                         ("usr/share" "share"))
      ;; yaakprotoc is statically linked and needs no patching.
      #:patchelf-plan
      `'(("usr/bin/yaak-app-client" ,%yaak-libs))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'discard-broken-vendored-node
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((yaaknode (string-append (assoc-ref outputs "out")
                                             "/lib/yaak/vendored/node/yaaknode")))
                (delete-file yaaknode)
                (call-with-output-file yaaknode
                  (lambda (port)
                    (format port "#!~a\nexec ~a \"$@\"\n"
                            #$(file-append bash-minimal "/bin/sh")
                            #$(file-append (this-package-input "node") "/bin/node"))))
                (chmod yaaknode #o755)))))))
    (inputs (cons node (map specification->package %yaak-libs)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://yaak.app")
    (synopsis "Desktop API client for REST, GraphQL, SSE, WebSocket, and gRPC")
    (description
     "Yaak is a desktop API client for REST, GraphQL, Server-Sent Events,
WebSocket, and gRPC.  Requests and environments are stored as plain text, so
projects can be versioned in Git like any other source file.  This package
installs the upstream prebuilt Debian package.")
    (license license:expat)))
