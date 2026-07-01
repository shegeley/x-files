(define-module (x-files packages mcp yandex-cloud)
  #:use-module ((guix packages)             #:select (package origin base32))
  #:use-module ((guix download)             #:select (url-fetch))
  #:use-module ((guix build-system trivial) #:select (trivial-build-system))
  #:use-module (guix gexp)
  #:use-module ((guix licenses)             #:prefix license:)

  #:export (yandex-cloud-mcp))

;; Yandex Cloud MCP.  The @code{@yandex-cloud/mcp} npm package is only a thin
;; launcher that downloads and spawns a native binary from Yandex Object
;; Storage; we skip npm entirely and repackage that binary — a single static
;; Go ELF, so installing is a plain copy.  One binary serves every Yandex Cloud
;; MCP server, selected with @code{-s <server>}.
(define-public yandex-cloud-mcp
  (let ((version "1.0.1"))
    (package
      (name "yandex-cloud-mcp")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://storage.yandexcloud.net/yandexcloud-mcp/"
                             version "/ycmcp_v" version "_linux_x64"))
         (file-name (string-append "ycmcp-" version))
         (sha256
          (base32 "0vb9bcwc8m94sflx1298ds3gwq2nsj1qpb5dz48zc7hkdmnvd91f"))))
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((bin    (string-append #$output "/bin"))
                   (src    (assoc-ref %build-inputs "source"))
                   (target (string-append bin "/ycmcp")))
              (mkdir-p bin)
              (copy-file src target)
              (chmod target #o755)))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://github.com/yandex-cloud/mcp")
      (synopsis "MCP servers for Yandex Cloud (toolkit, search, docs, …)")
      (description
       "Model Context Protocol (MCP) server for Yandex Cloud, shipped upstream
as the @command{@yandex-cloud/mcp} npm launcher that fetches a native binary;
this package repackages that static Go binary directly.  It is an authenticated
proxy to Yandex Cloud's hosted MCP servers: one binary serves every server,
chosen with @code{-s <server>} — @code{toolkit} (deploy to
Compute/VPC/IAM/Object Storage/YDB), @code{search} (Yandex Search API),
@code{documentation}, @code{datacatalog-consumer}, @code{apigateway},
@code{containers}, @code{functions}, @code{mcpgateway}, @code{triggers} and
@code{workflows}.  Run e.g. @command{ycmcp -s toolkit}; authenticate with a
Yandex Cloud CLI profile or OAuth.")
      (license license:asl2.0))))
