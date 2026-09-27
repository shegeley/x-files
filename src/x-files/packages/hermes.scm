(define-module (x-files packages hermes)
  #:use-module ((guix packages)
                #:select (package
                           origin base32 package-propagated-inputs
                           modify-inputs replace))
  #:use-module (guix gexp)
  #:use-module ((guix git-download)
                #:select (git-fetch git-reference git-file-name))
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((guix build-system pyproject)
                #:select (pyproject-build-system))

  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages python-web)
                #:select (python-openai python-httpx python-requests
                                        python-fastapi python-uvicorn
                                        python-websockets))
  #:use-module ((gnu packages python-crypto)
                #:select (python-certifi python-cryptography))
  #:use-module ((gnu packages python-xyz)
                #:select (python-dotenv python-fire
                                        python-rich
                                        python-tenacity
                                        python-pyyaml
                                        python-jinja2
                                        python-pydantic
                                        python-prompt-toolkit
                                        python-croniter
                                        python-snowballstemmer
                                        python-markdown
                                        python-pyjwt
                                        python-psutil
                                        python-pillow
                                        python-pillow-heif
                                        python-multipart
                                        python-ptyprocess))
  #:use-module ((gnu packages python-build)
                #:select (python-packaging python-pathspec python-setuptools))
  #:use-module ((gnu packages python-xyz)
                #:select (python-tomlkit))
  #:use-module ((x-files packages python-agents)
                #:select (python-anthropic python-agent-client-protocol
                                           python-mcp-2
                                           python-starlette-hermes))
  #:use-module ((gnu packages check)
                #:select (python-pytest python-pytest-asyncio))
  #:use-module ((gnu packages python)
                #:select (python))
  #:use-module ((gnu packages nss) #:select (nss-certs-for-test))
  #:use-module ((gnu packages serialization)
                #:select (python-ruamel.yaml)))

;; Headless gateway and ACP adapter; the Electron/Node frontends are separate.
;; Native nemo-relay and firecrawl-anydoc have supported reduced-capability
;; fallbacks and are not installed.  Lazy pip installation is disabled.
(define-public hermes-agent
  (package
    (name "hermes-agent")
    (version "0.21.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NousResearch/hermes-agent")
             (commit (string-append "v2026.9.21"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wm8pczg38vv9d8n0q9qxjgyi6ssj66gs1wlfs6dip9v76ag55yj"))
       (patches (map (lambda (name)
                       (search-path %load-path
                                    (string-append "x-files/packages/aux/"
                                                   name)))
                     '("hermes-agent-codex-cli-auth.patch"
                       "hermes-agent-guix-install.patch"
                       "hermes-agent-codex-tools.patch")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Hermes parses -p as a profile at import time; pytest_guix's -p
      ;; injection collides with it.  Use Python's pytest entry point.
      #:test-backend #~'custom
      #:test-flags
      #~(list "-m" "pytest" "-vv"
              "tests/agent/test_codex_app_server_integration.py"
              "tests/agent/test_codex_app_server_lifecycle.py"
              "tests/agent/test_codex_app_server_thread_resume.py"
              "tests/agent/transports/test_codex_app_server_session.py"
              "tests/agent/transports/test_codex_app_server_runtime.py"
              "tests/hermes_cli/test_managed_scope_overlay.py"
              "tests/hermes_cli/test_managed_scope_writeguard.py"
              "tests/test_guix.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'guix-metadata-and-tests
            (lambda _
              (invoke "python3"
                      #$(local-file (search-path %load-path
                                     "x-files/packages/aux/hermes-build-metadata.py")))
              (copy-file #$(local-file (search-path %load-path
                                        "x-files/packages/aux/hermes-guix-test.py"))
                         "tests/test_guix.py")
              (mkdir-p "test-home")
              (setenv "HOME"
                      (string-append (getcwd) "/test-home"))
              (setenv "SSL_CERT_FILE"
                      #$(file-append nss-certs-for-test
                                     "/etc/ssl/certs/ca-certificates.crt"))))
          (add-before 'build 'set-hermes-nix-build
            ;; setup.py hard-blocks `pip wheel'/`build' unless this is
            ;; set, to funnel users toward the shell installer, Docker,
            ;; or upstream's own Nix flake -- HERMES_NIX_BUILD=1 is the
            ;; escape hatch upstream's own nix/python.nix uses for
            ;; exactly this "reproducible package manager" scenario.
            (lambda _
              (setenv "HERMES_NIX_BUILD" "1")))
          (add-after 'install 'install-data
            (lambda _
              (for-each (lambda (directory)
                          (copy-recursively directory
                                            (string-append #$output
                                             "/share/hermes-agent/" directory)))
                        '("skills" "optional-skills" "plugins" "locales"
                          "optional-mcps"))
              (let ((root (dirname (car (find-files #$output
                                                    "^hermes_constants\\.py$")))))
                (call-with-output-file (string-append root "/.install_method")
                  (lambda (port)
                    (display "guix\n" port))))))
          (add-after 'wrap 'wrap-data-paths
            (lambda _
              (let ((variables
                     (append
                      (map
                       (lambda (entry)
                         `(,(car entry) =
                           (,(string-append #$output "/share/hermes-agent/"
                                            (cdr entry)))))
                       '(("HERMES_BUNDLED_SKILLS" . "skills")
                         ("HERMES_OPTIONAL_SKILLS" . "optional-skills")
                         ("HERMES_BUNDLED_PLUGINS" . "plugins")
                         ("HERMES_BUNDLED_LOCALES" . "locales")
                         ("HERMES_OPTIONAL_MCPS" . "optional-mcps")))
                      (list '("HERMES_DISABLE_LAZY_INSTALLS" = ("1"))
                            `("HERMES_PYTHON" =
                              (,(string-append #$python "/bin/python3")))))))
                (for-each
                 (lambda (command)
                   (apply wrap-program (string-append #$output "/bin/" command)
                          variables))
                 '("hermes" "hermes-agent" "hermes-acp"))))))))
    (inputs (list bash-minimal))
    (native-inputs (list python-setuptools python-tomlkit python-packaging
                         python-pytest python-pytest-asyncio))
    (propagated-inputs (list python-openai
                             python-anthropic
                             python-agent-client-protocol
                             python-mcp-2
                             python-certifi
                             python-dotenv
                             python-fire
                             python-httpx
                             python-rich
                             python-tenacity
                             python-pyyaml
                             python-ruamel.yaml
                             python-requests
                             python-jinja2
                             python-pydantic
                             python-prompt-toolkit
                             python-croniter
                             python-snowballstemmer
                             python-packaging
                             python-markdown
                             python-pyjwt
                             python-cryptography
                             python-psutil
                             python-websockets
                             python-pathspec
                             python-pillow
                             python-pillow-heif
                             (package
                               (inherit python-fastapi)
                               (propagated-inputs
                                (modify-inputs (package-propagated-inputs python-fastapi)
                                                    (replace
                                                     "python-starlette"
                                                     python-starlette-hermes))))
                             python-uvicorn
                             python-multipart
                             python-ptyprocess))
    (home-page "https://github.com/NousResearch/hermes-agent")
    (synopsis "Self-improving AI agent gateway (headless dashboard server)")
    (description
     "hermes-agent is NousResearch's AI agent.  This package provides only
its Python gateway -- the JSON-RPC/@code{WebSocket} server started with
@code{hermes serve} (or @code{hermes dashboard}), which is what
@code{emacs-hermes} and other API clients talk to.  The Electron desktop
app and the Node/Ink @code{ui-tui} terminal frontend are not built.")
    (license license:expat)))
