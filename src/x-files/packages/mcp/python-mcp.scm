(define-module (x-files packages mcp python-mcp)
  #:use-module ((guix packages)              #:select (package
                                                       package-propagated-inputs
                                                       modify-inputs))
  #:use-module ((gnu packages python-web)    #:select (python-mcp
                                                       python-httpx-sse
                                                       python-sse-starlette
                                                       python-starlette
                                                       python-uvicorn))
  #:use-module ((gnu packages python-xyz)    #:select (python-anyio
                                                       python-pydantic-settings
                                                       python-pyjwt
                                                       python-multipart
                                                       python-typing-inspection
                                                       python-jsonschema))
  #:use-module ((gnu packages python-build)  #:select (python-typing-extensions)))

;; Guix's `python-mcp' only propagates httpx and pydantic, but FastMCP (which
;; MCP servers like yandex-tracker run) needs the full server-side stack.  This
;; variant appends mcp's remaining runtime deps so the closure is complete.
;;
;; It carries a DISTINCT name ("python-mcp-full") on purpose: an inherited
;; variant that kept the base name "python-mcp" collides in a profile with
;; guix's plain `python-mcp' (pulled by other MCP servers, e.g.
;; python-mcp-google-sheets) — two different store items, same name.  A distinct
;; name lets both coexist.
(define-public python-mcp-full
  (package
    (inherit python-mcp)
    (name "python-mcp-full")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-mcp)
       (append python-anyio
               python-httpx-sse
               python-jsonschema
               python-pydantic-settings
               python-pyjwt
               python-multipart
               python-sse-starlette
               python-starlette
               python-typing-extensions
               python-typing-inspection
               python-uvicorn)))))
