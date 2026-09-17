(define-module (x-files packages emacs mcp-server-lib)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)           #:select (package origin base32))
  #:use-module ((guix gexp)               #:select (gexp))
  #:use-module ((guix git-download)       #:select (git-fetch git-reference git-file-name))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))

  #:export (emacs-mcp-server-lib))

(define emacs-mcp-server-lib
  (package
    (name "emacs-mcp-server-lib")
    (version "0.4.0")
    ;; Overrides the same-named package in (gnu packages emacs-xyz), which
    ;; is stuck at 0.3.0, while emacs-org-mcp's Package-Requires wants >=
    ;; 0.4.0.  Channel packages shadow dependency-channel ones by name.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/laurynas-biveinis/mcp-server-lib.el")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s81xi9k4jfz49a1zqnw8rr5q20jbp0840qy59974h4r3vkz1kf1"))))
    (build-system emacs-build-system)
    (arguments
     (list
      ;; ERT suite compares org-generated day-of-week names against English
      ;; abbreviations, so it fails under non-C time locales; passes under
      ;; LC_TIME=C.
      #:tests? #f
      ;; emacs-build-system installs only *.el/*.elc by default; the stdio
      ;; transport wrapper is a runtime artifact (the one
      ;; `mcp-server-lib-install' copies out), so ship it too.
      #:include #~(list "^[^/]*.el$" "^emacs-mcp-stdio.sh$")
      #:exclude #~(list "^[^/]*-test.el$" "^emacs-mcp-stdio-test.sh$")))
    (home-page "https://github.com/laurynas-biveinis/mcp-server-lib.el")
    (synopsis "Model Context Protocol server library")
    (description
     "This library enables Emacs packages to expose their
functionality to AI applications via the @acronym{MCP, Model Context
Protocol}.  The library handles JSON-RPC 2.0 communication, manages
tool and resource registration, and provides error handling suitable
for LLM interactions.  See https://modelcontextprotocol.io/ for the
protocol specification.")
    (license license:gpl3+)))
