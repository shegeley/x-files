(define-module (x-files packages nixd)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix build-system meson) #:select (meson-build-system))
  #:use-module ((guix gexp) #:select (gexp))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages package-management) #:select (nix))
  #:use-module ((gnu packages llvm) #:select (llvm))
  #:use-module ((gnu packages boost) #:select (boost))
  #:use-module ((gnu packages cpp) #:select (nlohmann-json))
  #:use-module ((gnu packages check) #:select (googletest))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages build-tools) #:select (ninja))
  #:use-module ((gnu packages backup) #:select (libarchive))
  #:use-module ((gnu packages compression) #:select (brotli))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module ((gnu packages crypto) #:select (libsodium libblake3))
  #:use-module ((gnu packages hardware) #:select (libcpuid))
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module ((gnu packages linux) #:select (libseccomp))
  #:use-module ((gnu packages sqlite) #:select (sqlite))
  #:use-module ((gnu packages version-control) #:select (libgit2))
  #:use-module ((gnu packages markup) #:select (lowdown))
  #:use-module ((gnu packages libedit) #:select (editline))
  #:use-module ((gnu packages bdw-gc) #:select (libgc))
  #:export (nixd))

;;; nixd — a Nix language server, evaluation-backed via libnixexpr (the same
;;; C++ library `nix' itself is built from), so completion/diagnostics reflect
;;; real evaluation against a live nix store/daemon rather than static
;;; scope analysis.  Meson build; links against the `nix-main', `nix-expr',
;;; `nix-cmd', `nix-flake' pkg-config modules `nix' already exports.

(define nixd
  (package
    (name "nixd")
    (version "2.9.2")
    (source
     (origin
       (method url-fetch)
       ;; Pinned to the commit the "2.9.2" annotated tag resolves to (tags
       ;; are mutable refs; the commit is not).
       (uri (string-append
             "https://github.com/nix-community/nixd/archive/"
             "2307b620d3ba35fe4b0131ecbf70aea63b63e82e.tar.gz"))
       (sha256 (base32 "08x8qspw1wk5a63v3vjh4a1x7mzyqf6v3gq28grpy4zjfnlj3ym1"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; libnixf/libnixt/nixd test suites need network-fetched nix
      ;; fixtures and a live evaluator; inappropriate for the build sandbox.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adapt-to-guix-nix-and-llvm-api-drift
            ;; nixd 2.9.2 was built against a nixpkgs snapshot where
            ;; nix::PrimOp::doc is std::optional<std::string> (has
            ;; .value_or()); Guix's nix 2.32.8 still has the older
            ;; `const char * doc = nullptr' -- a ternary is the
            ;; equivalent read on either type's C-string form.
            (lambda _
              (substitute*
                  "libnixf/src/Basic/PrimOpsInfoGen.cpp"
                (("PrimOp\\.doc\\.value_or\\(\"\"\\)")
                 "(PrimOp.doc ? PrimOp.doc : \"\")"))
              (substitute*
                  "nixd/lib/Eval/AttrSetProvider.cpp"
                (("PrimOp->doc\\.value_or\\(\"\"\\)")
                 "(PrimOp->doc ? PrimOp->doc : \"\")")
                ;; Same nix-API-ahead-of-Guix's-2.32.8 story: nixd expects
                ;; `Value::type' to be a compile-time template
                ;; (`type<true>()'); 2.32.8 still has it as a plain method
                ;; taking a runtime bool (`type(bool = false)').
                (("V\\.type<true>\\(\\)") "V.type(true)"))
              ;; Guix's llvm 22.1.8 inserted `vfs::FileSystem *VFS = nullptr'
              ;; before the `EnvVar' parameter of
              ;; cl::ParseCommandLineOptions; nixd's two callers still pass
              ;; positionally for the pre-VFS 5-argument signature, so their
              ;; EnvVar string now lands in the VFS slot -- add back the
              ;; missing nullptr.
              (substitute*
                  "nixd/tools/nixd.cpp"
                (("\"NIXD_FLAGS\"") "nullptr, \"NIXD_FLAGS\""))
              (substitute*
                  "nixd/tools/nixd-attrset-eval.cpp"
                (("\"NIXD_NIXPKGS_EVAL_FLAGS\"")
                 "nullptr, \"NIXD_NIXPKGS_EVAL_FLAGS\"")))))))
    ;; Beyond nixd's own direct deps (nix llvm boost nlohmann-json), `nix's
    ;; pkg-config modules (nix-util/nix-store/nix-fetchers/nix-cmd/nix-flake)
    ;; transitively `Requires:' this whole list -- meson's pkg-config lookup
    ;; walks the full chain even for deps nixd's own code never touches
    ;; directly, so every one of these needs its .pc on PKG_CONFIG_PATH.
    (inputs (list nix llvm boost nlohmann-json
                  libarchive brotli openssl libsodium libblake3 libcpuid
                  curl libseccomp sqlite libgit2 lowdown editline libgc))
    (native-inputs (list pkg-config ninja googletest))
    (synopsis "Nix language server with evaluation-backed diagnostics")
    (description
     "nixd is a Language Server Protocol implementation for the Nix
language.  Unlike scope-based servers, it links directly against
@code{libnixexpr} (the same library the @code{nix} command-line tool is
built from) to provide completion, diagnostics, and value previews backed
by real evaluation of your Nix expressions and flakes.")
    (home-page "https://github.com/nix-community/nixd")
    (license license:lgpl3+)))
