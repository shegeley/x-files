(define-module (x-files packages mcp browser)
  #:use-module ((gnu packages gcc) #:select (gcc))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"  . "rust-browser-mcp-x86_64-unknown-linux-gnu")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "0af8jf0rfy6zbrwcn4f5fb0v5zrmawlgslfmrpbfq5rj580qm2bw")))

(define-public rust-mcp-browser
  (let* [(target   (or (%current-target-system) (%current-system)))
         (mcp.bin  (assoc-ref target->bin-name target))
         (hash     (assoc-ref target->hash target))
         (version  "0.3.1")
         (uri      (string-append
                    "https://github.com/EmilLindfors/"
                    "rust-browser-mcp/releases/download/"
                    "v" version "/" mcp.bin
                    ".tar.gz"))]
    (package
      (name "rust-mcp-browser")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:strip-binaries?   #f
        #:patchelf-plan     #~'(("rust-browser-mcp" ("gcc" "openssl")))
        #:install-plan      #~'(("rust-browser-mcp" "/bin/"))
        #:validate-runpath? #f))
      (inputs (list (list gcc "lib") openssl))
      (supported-systems targets)
      (home-page "https://github.com/EmilLindfors/rust-browser-mcp")
      (synopsis "WebDriver MCP Server")
      (description "Professional browser automation for Claude and Claude Code via the Model Context Protocol (MCP). Features enterprise-grade performance monitoring, multi-session support, and zero-latency driver management.")
      ;; license is MIT. written in README
      (license license:expat))))
