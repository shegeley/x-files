(define-module (x-files packages emacs org-mcp)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)           #:select (package origin base32))
  #:use-module ((guix gexp)               #:select (gexp))
  #:use-module ((guix git-download)       #:select (git-fetch git-reference git-file-name))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((x-files packages emacs mcp-server-lib)
                #:select (emacs-mcp-server-lib))

  #:export (emacs-org-mcp))

(define emacs-org-mcp
  (package
    (name "emacs-org-mcp")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/laurynas-biveinis/org-mcp")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b9a29rp503h4xddrm7mcamqdnqsfn6g2nzj2m38l4v9pizxligb"))))
    (build-system emacs-build-system)
    ;; Same locale-sensitive ERT suite issue as emacs-mcp-server-lib.
    (arguments (list #:tests? #f
                     #:exclude #~(list "^[^/]*-test.el$")))
    ;; Hard dependency per Package-Requires; propagate so both end up in the
    ;; same profile and `mcp-server-lib-install' can find the stdio wrapper.
    (propagated-inputs (list emacs-mcp-server-lib))
    (home-page "https://github.com/laurynas-biveinis/org-mcp")
    (synopsis "Model Context Protocol server for Org-mode")
    (description
     "This package implements a @acronym{MCP, Model Context Protocol} server
for Org-mode, letting MCP clients such as AI assistants work with Org files
through a structured API: read raw files, outlines, headlines and nodes by
Org ID; grep and agenda views; and write tools to add and update TODOs, edit
headlines and bodies, set SCHEDULED/DEADLINE timestamps, refile and archive
subtrees.  The served files are restricted to the absolute paths listed in
@code{org-mcp-allowed-files}.")
    (license license:gpl3+)))
