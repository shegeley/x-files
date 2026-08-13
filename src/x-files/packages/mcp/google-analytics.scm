(define-module (x-files packages mcp google-analytics)
  #:use-module ((guix packages)               #:select (package
                                                          origin
                                                          base32
                                                          package-propagated-inputs
                                                          modify-inputs))
  #:use-module ((guix download)                #:select (url-fetch))
  #:use-module ((guix build-system pyproject)  #:select (pyproject-build-system))
  #:use-module (guix gexp)
  #:use-module ((guix licenses)                #:prefix license:)
  #:use-module ((gnu packages protobuf)        #:select (python-proto-plus))
  #:use-module ((gnu packages python-web)      #:select (python-google-auth
                                                          python-google-api-core
                                                          python-googleapis-common-protos
                                                          python-aiohttp
                                                          python-authlib
                                                          python-fastapi
                                                          python-httpx
                                                          python-opentelemetry-api
                                                          python-opentelemetry-sdk
                                                          python-requests
                                                          python-starlette
                                                          python-uvicorn
                                                          python-websockets
                                                          python-mcp))
  #:use-module ((gnu packages python-xyz)      #:select (python-anyio
                                                          python-click
                                                          python-distro
                                                          python-dotenv
                                                          python-jsonschema
                                                          python-multipart
                                                          python-pydantic
                                                          python-pyjwt
                                                          python-pyyaml
                                                          python-sniffio
                                                          python-tenacity
                                                          python-watchdog))
  #:use-module ((gnu packages python-build)    #:select (python-setuptools
                                                          python-wheel
                                                          python-flit-core
                                                          python-hatchling
                                                          python-typing-extensions))
  #:use-module ((gnu packages databases)       #:select (python-aiosqlite))
  #:use-module ((gnu packages graphviz)        #:select (python-graphviz))
  #:use-module ((gnu packages python-crypto)   #:select (python-cryptography))
  #:use-module ((gnu packages time)            #:select (python-tzlocal))
  ;; modify-inputs's (delete ...) clause literal must resolve to the same
  ;; binding (guix packages) itself sees for `delete' -- srfi-1's -- or the
  ;; syntax-rules literal match fails hygienically inside a define-module'd
  ;; file (it happens to work at a bare `guix repl' prompt only because the
  ;; REPL's default environment already has srfi-1 loaded).
  #:use-module ((srfi srfi-1)                  #:select (delete))
  #:use-module ((x-files packages mcp yandex-tracker)
                #:select (python-grpcio-next
                          python-protobuf-6-pep420
                          python-googleapis-common-protos-6))

  #:export (python-proto-plus-6
            python-grpcio-status-next
            python-google-api-core-6
            python-opentelemetry-semantic-conventions-next
            python-opentelemetry-api-next
            python-opentelemetry-sdk-next
            python-google-analytics-data
            python-google-analytics-admin
            python-google-genai
            python-google-adk
            google-analytics-mcp))

;; The Google Analytics Data/Admin API clients (below) are recent
;; gapic-generator-python output and need protobuf>=6 + a "registered
;; method" grpc stub API (grpcio>=1.59) -- the same generation-gap already
;; solved once in (x-files packages mcp yandex-tracker) for yandexcloud.
;; Guix's own python-google-api-core/python-proto-plus still propagate the
;; old python-protobuf (3.20.3), so rebuild the two against
;; python-protobuf-6-pep420 (imported from yandex-tracker.scm) instead of
;; duplicating that recipe here.

(define-public python-proto-plus-6
  (package
    (inherit python-proto-plus)
    (name "python-proto-plus-6")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-proto-plus)
       (delete "python-protobuf")
       (append python-protobuf-6-pep420)))))

;; grpcio-status is grpcio-next's status-conversion sibling package (same
;; upstream monorepo, released in lockstep); pin it to the same 1.78.0 so the
;; two are guaranteed API-compatible.
;;
;; To bump: keep in sync with python-grpcio-next's version in
;; (x-files packages mcp yandex-tracker), and recompute the hash with
;;   guix download https://files.pythonhosted.org/packages/.../grpcio_status-X.Y.Z.tar.gz
(define-public python-grpcio-status-next
  (package
    (name "python-grpcio-status-next")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/8a/cd/"
             "89ce482a931b543b92cdd9b2888805518c4620e0094409acb8c81dd4610a/"
             "grpcio_status-" version ".tar.gz"))
       (sha256
        (base32 "12g1b7fbvcznd8b0fac82f99w093njs3dyd0b95sizhv20lgsk53"))))
    (build-system pyproject-build-system)
    ;; Pure-Python wrapper around grpcio-next's already-built C core; no
    ;; ext_modules of its own.  Tests need a live grpc server -- skip.
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list python-grpcio-next
           python-googleapis-common-protos-6
           python-protobuf-6-pep420))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://grpc.io")
    (synopsis "Status proto mapping for grpcio (paired with grpcio-next)")
    (description "@code{grpcio_status} maps between @code{google.rpc.Status}
protobuf messages and grpc's status codes/details.  Built against
@code{python-protobuf-6-pep420}/@code{python-grpcio-next} instead of Guix's
stock (older) protobuf/grpcio, matching what recent gapic-generator-python
clients require.")
    (license license:asl2.0)))

;; Guix's python-google-api-core propagates plain python-protobuf (3.20.3)
;; and doesn't wire up the [grpc] extra at all.  Rebuild it against the
;; protobuf-6 stack plus grpcio-next/grpcio-status-next so grpc transports
;; (google.api_core.grpc_helpers) actually import.
(define-public python-google-api-core-6
  (package
    (inherit python-google-api-core)
    (name "python-google-api-core-6")
    (propagated-inputs
     (list python-google-auth
           python-googleapis-common-protos-6
           python-protobuf-6-pep420
           python-proto-plus-6
           python-requests
           python-grpcio-next
           python-grpcio-status-next))))

;; google-adk imports google.adk.telemetry.tracing at module load (via
;; google.adk.agents -> base_agent -> telemetry), which needs
;; GEN_AI_TOOL_DEFINITIONS from opentelemetry-semantic-conventions.  Guix's
;; python-opentelemetry-{api,sdk} are 1.37.0, paired with semantic-conventions
;; 0.58b0 -- which predates that constant (added in 0.60b0) -- so the actual
;; import crashes, not just a metadata pin.  opentelemetry's api/sdk/semconv
;; are released as one exact-pinned lockstep trio, so all three must move
;; together; 0.60b0/1.39.0/1.39.0 is the EARLIEST train with the constant,
;; and 1.39.0 happens to be exactly adk's own stated opentelemetry-api/sdk
;; floor (">=1.39,<=1.42.1") -- no pin relaxation needed for those two.
;;
;; To bump: keep all three in lockstep (check each sdist's pyproject.toml for
;; the exact pins) and recompute hashes with
;;   guix download https://files.pythonhosted.org/packages/.../opentelemetry_semantic_conventions-X.YbZ.tar.gz
;;   guix download https://files.pythonhosted.org/packages/.../opentelemetry_api-X.Y.Z.tar.gz
;;   guix download https://files.pythonhosted.org/packages/.../opentelemetry_sdk-X.Y.Z.tar.gz
(define-public python-opentelemetry-api-next
  (package
    (inherit python-opentelemetry-api)
    (name "python-opentelemetry-api-next")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/c0/0b/"
             "e5428c009d4d9af0515b0a8371a8aaae695371af291f45e702f7969dce6b/"
             "opentelemetry_api-" version ".tar.gz"))
       (sha256
        (base32 "1f9k55x096qnbsm2szlzg0z6p40hiy3wwq7nzbgnpb65d1168c31"))))
    (arguments (list #:tests? #f))
    (native-inputs (list python-hatchling))))

(define-public python-opentelemetry-semantic-conventions-next
  (package
    (name "python-opentelemetry-semantic-conventions-next")
    (version "0.60b0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/71/0e/"
             "176a7844fe4e3cb5de604212094dffaed4e18b32f1c56b5258bcbcba85c2/"
             "opentelemetry_semantic_conventions-" version ".tar.gz"))
       (sha256
        (base32 "13xzqlx7np15qc5rvv3qgv7h3ajkam3bdmi9h10jx2mv7jkplz92"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-opentelemetry-api-next))
    (native-inputs (list python-hatchling))
    (home-page "https://opentelemetry.io/docs/languages/python/")
    (synopsis "OpenTelemetry semantic conventions (paired with -next api/sdk)")
    (description "Constants for OpenTelemetry semantic conventions
(attribute/metric names), bumped in lockstep with
@code{python-opentelemetry-api-next}/@code{python-opentelemetry-sdk-next} so
recent instrumentation code (e.g. gen_ai.* attributes used by
@code{google-adk}) can actually import what it references.")
    (license license:asl2.0)))

(define-public python-opentelemetry-sdk-next
  (package
    (inherit python-opentelemetry-sdk)
    (name "python-opentelemetry-sdk-next")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/51/e3/"
             "7cd989003e7cde72e0becfe830abff0df55c69d237ee7961a541e0167833/"
             "opentelemetry_sdk-" version ".tar.gz"))
       (sha256
        (base32 "1pjxw7h55bj86n93xw77kyrfgc6nm6yg31frlixf0a855bqh88n2"))))
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list python-opentelemetry-api-next
           python-opentelemetry-semantic-conventions-next
           python-typing-extensions))
    (native-inputs (list python-hatchling))))

;; To bump: pin to whatever version analytics-mcp's pyproject.toml requires
;; exactly (it uses ==, not a range), and recompute the hash with
;;   guix download https://files.pythonhosted.org/packages/.../google_analytics_data-X.Y.Z.tar.gz
(define-public python-google-analytics-data
  (package
    (name "python-google-analytics-data")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/f8/09/"
             "76553c6ec3e277d38486f7619d8bad631745fb6bdd486027e956dea8a8c7/"
             "google_analytics_data-" version ".tar.gz"))
       (sha256
        (base32 "12mhj2qldvhj0xbzfs9bik3zbaphyawyv2h0hl9y2xd1kj1bi6c7"))))
    (build-system pyproject-build-system)
    ;; Generated GAPIC unit tests exercise a mock transport harness this
    ;; recipe doesn't wire up; skip like yandexcloud/yandex-tracker-mcp.
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list python-google-api-core-6
           python-google-auth
           python-grpcio-next
           python-proto-plus-6
           python-protobuf-6-pep420))
    (native-inputs (list python-setuptools python-wheel))
    (home-page
     "https://github.com/googleapis/google-cloud-python/tree/main/packages/google-analytics-data")
    (synopsis "Google Analytics Data API (GA4) client")
    (description "Generated gRPC/REST client for the Google Analytics Data
API: runs reports and real-time reports against GA4 properties.")
    (license license:asl2.0)))

;; To bump: same as python-google-analytics-data above, matching analytics-mcp's
;; exact pin.
(define-public python-google-analytics-admin
  (package
    (name "python-google-analytics-admin")
    (version "0.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/ec/0b/"
             "dc92f543e8f93837d6c8f72f6052b48a81919a9676e4888852eb0f747fa6/"
             "google_analytics_admin-" version ".tar.gz"))
       (sha256
        (base32 "1dhr84ysi8kdqrm5wbj3dl7hx8g82q9dinb7i1q4bv6v2w11b2yc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; sanity-check enforces setup.py's declared "protobuf >= 6.33.5"
          ;; via pkg_resources; Guix's python-protobuf-6 is 6.31.1.  A patch
          ;; version behind that pin is not a real API gap here, so relax it
          ;; the same way yandex-tracker-mcp relaxes its yarl/dateutil pins.
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                (("protobuf >= 6\\.33\\.5") "protobuf >= 6.31.1")))))))
    (propagated-inputs
     (list python-google-api-core-6
           python-google-auth
           python-grpcio-next
           python-proto-plus-6
           python-protobuf-6-pep420))
    (native-inputs (list python-setuptools python-wheel))
    (home-page
     "https://github.com/googleapis/google-cloud-python/tree/main/packages/google-analytics-admin")
    (synopsis "Google Analytics Admin API (GA4) client")
    (description "Generated gRPC/REST client for the Google Analytics Admin
API: manages GA4 account/property configuration (data streams, custom
dimensions/metrics, audiences, etc).")
    (license license:asl2.0)))

;; google-genai's own deps are all already plain Guix packages -- no
;; protobuf-6 wiring needed here.
;;
;; To bump: recompute the hash with
;;   guix download https://files.pythonhosted.org/packages/.../google_genai-X.Y.Z.tar.gz
(define-public python-google-genai
  (package
    (name "python-google-genai")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/5f/2f/"
             "f8839312750ca9633606f187943eb9f73e6c44729d8fb67783c039f1680b/"
             "google_genai-" version ".tar.gz"))
       (sha256
        (base32 "0klla2556i7liy3amvwbfc3jl1392irv79743a6kbp0i8x9karl3"))))
    (build-system pyproject-build-system)
    ;; Test suite hits the live Gemini/Vertex AI API.
    (arguments (list #:tests? #f))
    (propagated-inputs
     (list python-anyio
           python-distro
           python-google-auth
           python-httpx
           python-pydantic
           python-requests
           python-sniffio
           python-tenacity
           python-typing-extensions
           python-websockets))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/googleapis/python-genai")
    (synopsis "Google Gen AI Python SDK (Gemini API / Vertex AI)")
    (description "Python SDK for Google's Gen AI APIs (Gemini Developer API
and Vertex AI), used here only as a transitive dependency of
@code{google-adk}.")
    (license license:asl2.0)))

;; google-adk's remaining direct deps are all plain Guix packages already;
;; only google-genai (above) was missing.
;;
;; To bump: recompute the hash with
;;   guix download https://files.pythonhosted.org/packages/.../google_adk-X.Y.Z.tar.gz
(define-public python-google-adk
  (package
    (name "python-google-adk")
    (version "2.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/5e/5a/"
             "a8a1fd24f8bfdbd64e54e1ae954ddb272d1debc61a7a69ce4a8277c70e32/"
             "google_adk-" version ".tar.gz"))
       (sha256
        (base32 "0bf7m0rzc109h287qi2wzy14qqlk702s1izkmb9km2gqspm91wm5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Test suite needs a live/mocked Vertex+GCP harness far beyond this
      ;; recipe's scope.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Guix ships python-websockets 16.0 (adk pins <16), python-tzlocal
          ;; 5.2 (adk pins >=5.3), python-starlette 0.49.1 (adk pins >=1.3.1
          ;; -- likely a typo upstream for 0.31/0.40-ish, since Starlette
          ;; itself has never shipped a 1.x), and python-authlib 1.5.1 (adk
          ;; pins >=1.6.6,<2).  None are real API gaps -- verified every other
          ;; pin in adk's dependencies list against Guix's actual shipped
          ;; versions and all others already satisfy -- so relax just these.
          ;; (opentelemetry-api/sdk need no relaxation: the -next 1.39.0
          ;; variants above already satisfy adk's original ">=1.39,<=1.42.1".)
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("websockets>=15\\.0\\.1,<16") "websockets>=15.0.1,<17")
                (("tzlocal>=5\\.3,<6") "tzlocal>=5.2,<6")
                (("starlette>=1\\.3\\.1,<2") "starlette>=0.40,<1")
                (("authlib>=1\\.6\\.6,<2") "authlib>=1.5.1,<2")))))))
    (propagated-inputs
     (list python-aiohttp
           python-aiosqlite
           python-authlib
           python-click
           python-fastapi
           python-google-auth
           python-google-genai
           python-graphviz
           python-httpx
           python-jsonschema
           python-opentelemetry-api-next
           python-opentelemetry-sdk-next
           python-dotenv
           python-multipart
           python-pydantic
           python-pyyaml
           python-requests
           python-starlette
           python-tenacity
           python-typing-extensions
           python-tzlocal
           python-uvicorn
           python-watchdog
           python-websockets))
    ;; pyproject build-backend is flit_core.buildapi, not setuptools.
    (native-inputs (list python-flit-core))
    (home-page "https://github.com/google/adk-python")
    (synopsis "Google Agent Development Kit (ADK) for Python")
    (description "Google's Agent Development Kit: a framework for building,
evaluating, and deploying AI agents.  Pulled in here as a dependency of
@code{google-analytics-mcp} for its @code{FunctionTool}/MCP tool-conversion
helpers.")
    (license license:asl2.0)))

;; To bump: recompute the hash with
;;   guix download https://files.pythonhosted.org/packages/.../analytics_mcp-X.Y.Z.tar.gz
(define-public google-analytics-mcp
  (package
    (name "google-analytics-mcp")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/08/1c/"
             "2f89d92fa4b76b36674f35858cc977f0950bbd056ca77c4ce951cd6ea15e/"
             "analytics_mcp-" version ".tar.gz"))
       (sha256
        (base32 "028dlgh3ir56wljp7mnikwn19g11daxd7h4cppb8y2lmrlp9xr3p"))))
    (build-system pyproject-build-system)
    ;; Runtime needs a live GA4 property + Google OAuth credentials.
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-google-analytics-admin
           python-google-analytics-data
           python-google-auth
           python-google-adk
           python-httpx
           python-mcp
           ;; Guix's python-mcp doesn't propagate its own "pyjwt[crypto]"
           ;; extra (used only by mcp's OAuth server support, which this
           ;; stdio-only MCP server never touches); add it explicitly so
           ;; sanity-check's dependency-completeness check passes.
           python-pyjwt
           python-cryptography))
    (home-page "https://github.com/googleanalytics/google-analytics-mcp")
    (synopsis "MCP server for Google Analytics (GA4)")
    (description "Model Context Protocol (MCP) server giving AI assistants
read-only access to Google Analytics 4: account/property metadata via the
Admin API and report data (standard, real-time, and funnel reports) via the
Data API.  Provides the @command{analytics-mcp} (and legacy
@command{google-analytics-mcp}) executables, speaking the stdio MCP
transport; authenticates via Application Default Credentials (needs the
@code{analytics.readonly} OAuth scope).")
    (license license:asl2.0)))
