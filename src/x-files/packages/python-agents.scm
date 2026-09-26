(define-module (x-files packages python-agents)
  #:use-module ((guix packages)
                #:select (package
                           origin base32 package-propagated-inputs
                           modify-inputs replace))
  #:use-module ((guix download)
                #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module ((guix build-system pyproject)
                #:select (pyproject-build-system pypi-uri))
  #:use-module ((gnu packages python-build)
                #:select (python-hatchling python-hatch-fancy-pypi-readme
                                           python-pdm-backend
                                           python-typing-extensions))
  #:use-module ((gnu packages python-web)
                #:select (python-httpcore python-jiter
                                          python-httpx
                                          python-h11
                                          python-truststore
                                          python-mcp
                                          python-starlette
                                          python-sse-starlette
                                          python-opentelemetry-api
                                          python-uvicorn))
  #:use-module ((gnu packages python-xyz)
                #:select (python-anyio python-distro
                                       python-docstring-parser
                                       python-sniffio
                                       python-idna
                                       python-jsonschema
                                       python-pydantic
                                       python-pyjwt
                                       python-multipart
                                       python-tomlkit
                                       python-typing-inspection))
  #:use-module ((gnu packages python-crypto)
                #:select (python-cryptography)))

;; Release sdists carry their resolved metadata.  Use it instead of a Git/uv
;; version probe, which has neither a checkout nor a resolver in the sandbox.
(define %release-metadata-phase
  #~(lambda _
      (invoke "python3"
              #$(local-file (search-path %load-path
                             "x-files/packages/aux/python-release-metadata.py")))))

(define %release-arguments
  (list #:tests? #f
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'release-metadata
                       #$%release-metadata-phase))))

(define-public python-anthropic
  (package
    (name "python-anthropic")
    (version "0.87.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "anthropic" version))
       (sha256
        (base32 "1l02cwznbd2va8lh65y58n6pjfnhr2wyz5bgm3dc1lydacvyz3q9"))))
    (build-system pyproject-build-system)
    ;; Cloud-transport tests require additional provider SDKs.  Keep the
    ;; installed import/requirements check; Hermes tests client construction.
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme))
    (propagated-inputs (list python-anyio
                             python-distro
                             python-docstring-parser
                             python-httpx
                             python-jiter
                             python-pydantic
                             python-sniffio
                             python-typing-extensions))
    (home-page "https://github.com/anthropics/anthropic-sdk-python")
    (synopsis "Python client for Anthropic")
    (description
     "This package provides the Anthropic Python SDK, including
streaming, typed messages and synchronous and asynchronous clients.")
    (license license:expat)))

(define-public python-agent-client-protocol
  (package
    (name "python-agent-client-protocol")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "agent_client_protocol" version))
       (sha256
        (base32 "02z3kgj6vlszxkzddryv4rybkjhvssc59az5a920n3xgp65c8i7p"))))
    (build-system pyproject-build-system)
    ;; The release archive does not include upstream's test suite.
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-pdm-backend))
    (propagated-inputs (list python-pydantic))
    (home-page "https://github.com/agentclientprotocol/python-sdk")
    (synopsis "Agent Client Protocol Python SDK")
    (description
     "This package provides Python clients and servers for the
Agent Client Protocol, including typed messages and stdio transport.")
    (license license:asl2.0)))

(define-public python-httpcore2
  (package
    (inherit python-httpcore)
    (name "python-httpcore2")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpcore2" version))
       (sha256
        (base32 "1z4ywidmwayysw6yl057iq8q3smszvnpjm8ajf8ajlls6bgzxh3d"))))
    (arguments
     %release-arguments)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-tomlkit))
    (propagated-inputs (list python-h11 python-truststore python-anyio))))

(define-public python-httpx2
  (package
    (inherit python-httpx)
    (name "python-httpx2")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpx2" version))
       (sha256
        (base32 "01sv6z8f0lgm1amh0bbsgsgggj01wffc159vvnq6b12wxnd70c4b"))))
    (arguments
     %release-arguments)
    (native-inputs (list python-hatchling python-hatch-fancy-pypi-readme
                         python-tomlkit))
    (propagated-inputs (list python-anyio python-httpcore2 python-idna
                             python-truststore python-typing-extensions))))

(define-public python-mcp-types
  (package
    (name "python-mcp-types")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mcp_types" version))
       (sha256
        (base32 "0yc3dmw338dn9szyyxp2mwmx2d6shpppbfk6i2p636aw52wkknfp"))))
    (build-system pyproject-build-system)
    (arguments
     %release-arguments)
    (native-inputs (list python-hatchling python-tomlkit))
    (propagated-inputs (list python-pydantic python-typing-extensions))
    (home-page "https://github.com/modelcontextprotocol/python-sdk")
    (synopsis "Model Context Protocol Python types")
    (description
     "Typed Model Context Protocol messages shared by Python clients and servers.")
    (license license:expat)))

(define-public python-starlette-hermes
  (package
    (inherit python-starlette)
    (name "python-starlette-hermes")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "starlette" version))
       (sha256
        (base32 "1q142yki2vz3mda6nls69snn4hnx98xmkjrf1vkamyzjjcqj3l05"))))
    (arguments
     (list
      #:tests? #f))
    (native-inputs (list python-hatchling))
    (propagated-inputs (list python-anyio python-typing-extensions
                             python-httpx2))))

(define-public python-mcp-2
  (package
    (inherit python-mcp)
    (name "python-mcp-2")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mcp" version))
       (sha256
        (base32 "0a070m74y0qlbv9pgfrg8d438cglhq5wyqmw36xyiv0kbirhwi0g"))))
    (arguments
     %release-arguments)
    (native-inputs (list python-hatchling python-tomlkit))
    (propagated-inputs (list python-anyio
                             python-httpx2
                             python-jsonschema
                             python-mcp-types
                             python-opentelemetry-api
                             python-pydantic
                             python-pyjwt
                             python-cryptography
                             python-multipart
                             python-starlette-hermes
                             (package
                               (inherit python-sse-starlette)
                               (propagated-inputs
                                (modify-inputs
                                    (package-propagated-inputs python-sse-starlette)
                                  (replace "python-starlette"
                                           python-starlette-hermes))))
                             python-typing-extensions
                             python-typing-inspection
                             python-uvicorn))))
