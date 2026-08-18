(define-module (x-files packages vscode-js-debug)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))

  #:use-module ((contrib packages node-xyz) #:select (node-vscode-js-debug-1.86.0))
  #:use-module ((gnu packages node) #:select (node-lts))

  #:export (node-vscode-js-debug-latest))

;; A newer node-vscode-js-debug than rde's contrib/packages/node-xyz.scm
;; pins.  Used by (x-files features deno) and the dape-* debug-config
;; packages (dape-deno, dape-typescript) -- kept here, not in features/deno,
;; so those packages don't have to import a feature module just to get a
;; package definition (and so features/deno.scm and dape-deno.scm don't form
;; a module cycle importing each other).
(define node-vscode-js-debug-latest
  (let* [(version "1.97.1")
         (uri (string-append
               "https://github.com/microsoft/vscode-js-debug/"
               "releases/download/v" version
               "/js-debug-dap-v" version ".tar.gz"))
         (hash "135dj20maszb1xwsqq4mh3ah3rzbv2j3y066z56p4ilwbn4lgv9x")]
    (package
      (inherit node-vscode-js-debug-1.86.0)
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri uri)
         (sha256 (base32 hash))))
      (inputs (list node-lts)))))
