(define-module (x-files packages mcp filesystem)
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"  . "rust-mcp-filesystem-x86_64-unknown-linux-gnu")
    ("aarch64-linux" . "rust-mcp-filesystem-aarch64-unknown-linux-gnu")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "0db88slg0wfqrvr4lgj6ncfxwfpipm0djxyiaqwnlhpzb289rlwv")
    ("aarch64-linux" . "1jzk89kxpnryvzbbmb9ikkgrqhnd41wqccgf1xzwdbhbc6nmx3dw")))

(define-public rust-mcp-filesystem
  (let* [(target   (or (%current-target-system) (%current-system)))
         (mcp.bin  (assoc-ref target->bin-name target))
         (hash     (assoc-ref target->hash target))
         (version  "0.3.6")
         (uri      (string-append
                    "https://github.com/rust-mcp-stack/"
                    "rust-mcp-filesystem/"
                    "releases/download/v" version "/"
                    mcp.bin ".tar.gz"))]
    (package
      (name "rust-mcp-filesystem")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:strip-binaries?   #f
        #:patchelf-plan     #~'(("rust-mcp-filesystem" ("gcc")))
        #:install-plan      #~'(("rust-mcp-filesystem" "/bin/"))
        #:validate-runpath? #f))
      (inputs (list `(,gcc "lib")))
      (supported-systems targets)
      (home-page "https://github.com/rust-mcp-stack/rust-mcp-filesystem")
      (synopsis "Rust MCP Filesystem")
      (description "Rust MCP Filesystem is a blazingly fast,
asynchronous, and lightweight MCP (Model Context Protocol) server
designed for efficient handling of various filesystem operations. This
project is a pure Rust rewrite of the JavaScript-based
@modelcontextprotocol/server-filesystem, offering enhanced
capabilities, improved performance, and a robust feature set tailored
for modern filesystem interactions.")
      (license license:expat))))
