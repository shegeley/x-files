(define-module (x-files packages mcp yandex-tracker)
  #:use-module ((guix packages)                #:select (package
                                                          origin
                                                          base32
                                                          package-arguments
                                                          package-propagated-inputs
                                                          modify-inputs))
  #:use-module ((guix download)                #:select (url-fetch))
  #:use-module ((guix build-system pyproject)  #:select (pyproject-build-system))
  #:use-module ((guix utils)                   #:select (substitute-keyword-arguments))
  #:use-module (guix gexp)
  #:use-module ((guix licenses)                #:prefix license:)
  #:use-module ((gnu packages databases)       #:select (python-redis))
  #:use-module ((gnu packages python-crypto)   #:select (python-cryptography))
  #:use-module ((gnu packages protobuf)        #:select (python-protobuf-6))
  #:use-module ((gnu packages tls)             #:select (openssl))
  #:use-module ((gnu packages compression)     #:select (zlib))
  #:use-module ((gnu packages pkg-config)      #:select (pkg-config))
  #:use-module ((gnu packages python-web)      #:select (python-aiohttp
                                                          python-yarl
                                                          python-requests
                                                          python-mcp
                                                          python-httpx-sse
                                                          python-sse-starlette
                                                          python-starlette
                                                          python-uvicorn
                                                          python-googleapis-common-protos))
  #:use-module ((gnu packages python-xyz)      #:select (python-pydantic
                                                          python-pydantic-settings
                                                          python-thefuzz
                                                          python-pyjwt
                                                          python-deprecated
                                                          python-anyio
                                                          python-jsonschema
                                                          python-multipart
                                                          python-typing-inspection))
  #:use-module ((gnu packages python-build)    #:select (python-six
                                                          python-setuptools
                                                          python-wheel
                                                          python-typing-extensions))
  #:use-module ((gnu packages time)            #:select (python-dateutil))
  #:use-module ((x-files packages mcp python-mcp) #:select (python-mcp-full))

  #:export (python-aiocache
            python-grpcio-next
            python-protobuf-6-pep420
            python-googleapis-common-protos-6
            python-yandexcloud
            yandex-tracker-mcp))

(define-public python-aiocache
  (package
    (name "python-aiocache")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/7a/64/"
             "b945b8025a9d1e6e2138845f4022165d3b337f55f50984fbc6a4c0a1e355/"
             "aiocache-" version ".tar.gz"))
       (sha256
        (base32 "04s79ka7dcrwf4hkcz73n4x1ab2lkbssilfhl6bv8dnlyixv4a7m"))))
    (build-system pyproject-build-system)
    ;; Tests reach out to a live Redis/Memcached server.
    (arguments (list #:tests? #f))
    (native-inputs (list python-setuptools python-wheel))
    ;; [redis] extra: RedisOAuthStore and the cache backend need it.
    (propagated-inputs (list python-redis))
    (home-page "https://github.com/aio-libs/aiocache")
    (synopsis "Asyncio cache supporting multiple backends")
    (description "@code{aiocache} is an asyncio cache library supporting
in-memory, Redis and Memcached backends, with a common interface, decorators
and pluggable serializers.")
    (license license:bsd-3)))

;; yandexcloud 0.399's generated stubs use the registered-method gRPC API and
;; guard for grpcio>=1.78; Guix ships 1.52.  Build a matching grpcio from its
;; PyPI sdist with the vendored C-core (only openssl and zlib come from Guix).
(define-public python-grpcio-next
  (package
    (name "python-grpcio-next")
    (version "1.78.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/06/8a/"
             "3d098f35c143a89520e568e6539cc098fcd294495910e359889ce8741c84/"
             "grpcio-" version ".tar.gz"))
       (sha256
        (base32 "1igiqjlywfsvrh2slpq0p3293vrwhzxaalsg2xf3fvsli58vk0kk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure-grpc
            (lambda _
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_OPENSSL" "1")
              (setenv "GRPC_PYTHON_BUILD_SYSTEM_ZLIB" "1")
              ;; Python's sysconfig compiles extensions at -O3, which on grpc's
              ;; huge generated C++ takes ~10 min per file.  Runtime speed is
              ;; irrelevant for a token-exchange stub, so force -O1.
              (setenv "CFLAGS" "-O1")
              (setenv "CXXFLAGS" "-O1")
              (substitute* '("setup.py" "src/python/grpcio/commands.py")
                (("'cc'") "'gcc'")))))))
    (inputs (list openssl zlib))
    (native-inputs (list python-setuptools python-wheel pkg-config))
    (home-page "https://grpc.io")
    (synopsis "HTTP/2-based RPC framework (grpcio 1.78, bundled C-core)")
    (description "Python bindings for the gRPC framework, built at version 1.78
from the vendored C-core so that recent generated stubs (which require the
registered-method API) run correctly.")
    (license license:asl2.0)))

;; yandexcloud's gRPC stubs need protobuf 6; Guix's googleapis-common-protos
;; is 1.66 (capped <6) so bump it to 1.70 and rebuild against protobuf 6.
;;
;; protobuf-6's sdist ships a regular google/__init__.py (pkg_resources
;; namespace stub) that shadows PEP-420 namespace merging for every other
;; google.* package (e.g. google.api_core) on sys.path. Strip it so
;; everything merges via plain PEP 420 instead. Mirrors
;; (ai-cloud packages mcp-google-sheets)'s definitions of the same name —
;; identical recipe resolves to the identical store item, so the two
;; channels' packages don't collide when installed together.  Exported (along
;; with python-googleapis-common-protos-6 below) so other protobuf-6-needing
;; MCP packages -- e.g. (x-files packages mcp google-analytics) -- can reuse
;; the same store items instead of rebuilding an equivalent recipe.
(define-public python-protobuf-6-pep420
  (package
    (inherit python-protobuf-6)
    (arguments
     (substitute-keyword-arguments (package-arguments python-protobuf-6)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'strip-google-namespace-stub
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (delete-file
                 (string-append (site-packages inputs outputs)
                                "/google/__init__.py"))))))))))

(define-public python-googleapis-common-protos-6
  (package
    (inherit python-googleapis-common-protos)
    (version "1.70.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/39/24/"
             "33db22342cf4a2ea27c9955e6713140fedd51e8b141b5ce5260897020f1a/"
             "googleapis_common_protos-" version ".tar.gz"))
       (sha256
        (base32 "0mrjfp51f9vlwa1kz11dxam8m9lka7qzx57kz6a6aghmxbh486qf"))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-googleapis-common-protos)
       ;; A namespace-package compat test is unrelated to our runtime use.
       ((#:tests? _ #f) #f)))
    (propagated-inputs (list python-protobuf-6-pep420))))

(define-public python-yandexcloud
  (package
    (name "python-yandexcloud")
    (version "0.402.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/9c/c0/"
             "b019909268ee17eb9a76b4d6647d986e2b324a6f1b8258fc935cfe2effeb/"
             "yandexcloud-" version ".tar.gz"))
       (sha256
        (base32 "03mjvsnsx2izaph849k6kd2h6g5ki72r9bhwfb6zwcb5jzp3k5px"))))
    (build-system pyproject-build-system)
    ;; No tests shipped in the sdist; the generated gRPC stubs are import-only.
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'drop-grpcio-tools
            ;; grpcio-tools is a codegen-only dependency, unused at runtime.
            (lambda _
              (substitute* "pyproject.toml"
                (("[ ]*\"grpcio-tools>=[^\"]*\",\n") "")))))))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-cryptography
           python-grpcio-next
           python-protobuf-6-pep420
           python-googleapis-common-protos-6
           python-pyjwt
           python-requests
           python-six
           python-deprecated))
    (home-page "https://github.com/yandex-cloud/python-sdk")
    (synopsis "Yandex.Cloud SDK for Python")
    (description "Official Yandex.Cloud SDK for Python: gRPC clients for the
Yandex Cloud API plus helpers to obtain IAM tokens from OAuth tokens or service
account keys.")
    (license license:expat)))

;; python-mcp-full (the FastMCP-complete python-mcp variant, named distinctly to
;; avoid a profile clash with guix's plain `python-mcp') now lives in
;; (x-files packages mcp python-mcp) and is imported above.

(define-public yandex-tracker-mcp
  (package
    (name "yandex-tracker-mcp")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://files.pythonhosted.org/packages/dd/c5/"
             "c7ac8264ddb0f6f3057988ce31bbeef3d83fc99acd24e91d6ab16cd81287/"
             "yandex_tracker_mcp-" version ".tar.gz"))
       (sha256
        (base32 "0c3742yj0kzs6qb819b4vz3wld20dl459i1bi49v76qbsidwbsp4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Test group pulls pytest-xdist/aioresponses and hits the Tracker API.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; sanity-check loads the console entry point, which builds Settings()
          ;; at import time and aborts unless a Tracker token is configured.
          ;; The plain module import (also run by sanity-check) already passes.
          (delete 'sanity-check)
          (add-after 'unpack 'relax-requirements
            ;; Guix ships slightly older point releases with the same API.
            (lambda _
              (substitute* "pyproject.toml"
                (("yarl>=1\\.20\\.0") "yarl>=1.18.0")
                (("python-dateutil>=2\\.9\\.0\\.post0") "python-dateutil>=2.9.0")
                ;; The mcp CLI (typer) extra is unused; the server runs FastMCP.
                (("mcp\\[cli\\]>=1\\.21") "mcp>=1.21")))))))
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-aiocache
           python-aiohttp
           python-cryptography
           python-mcp-full
           python-pydantic
           python-pydantic-settings
           python-dateutil
           python-thefuzz
           python-yandexcloud
           python-yarl))
    (home-page "https://github.com/aikts/yandex-tracker-mcp")
    (synopsis "MCP server for Yandex Tracker")
    (description "Model Context Protocol (MCP) server that gives AI assistants
authenticated access to Yandex Tracker: issues, queues, comments, worklogs,
fields and search, with optional Redis caching and OAuth.  Provides the
@command{yandex-tracker-mcp} executable, speaking stdio, streamable-http or
SSE transports.")
    (license license:expat)))
