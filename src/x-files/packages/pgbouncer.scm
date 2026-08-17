(define-module (x-files packages pgbouncer)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix build-system gnu) #:select (gnu-build-system))
  #:use-module ((guix gexp) #:select (gexp))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages libevent) #:select (libevent))
  #:use-module ((gnu packages tls) #:select (openssl))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:export (pgbouncer))

;;; PgBouncer — a lightweight connection pooler for PostgreSQL.  Standard
;;; autotools build (a pre-generated `configure' ships in the release
;;; tarball, no autoreconf needed).  libevent is the only hard dependency;
;;; openssl is auto-detected by `configure' (AC_USUAL_TLS, default "auto")
;;; and enables TLS support when present, which we want.

(define pgbouncer
  (package
    (name "pgbouncer")
    (version "1.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pgbouncer/pgbouncer/releases/download/"
             "pgbouncer_" (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             "/pgbouncer-" version ".tar.gz"))
       (sha256 (base32 "0cmk72ifwfwq9259yymknzi34ma40dfmps6vwb4722px2d8x6jlj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; `make' unconditionally builds the man pages too (needs python3 +
      ;; pandoc, neither worth pulling in for a headless daemon) -- disabling
      ;; dist_man_MANS drops that whole dependency chain from both build and
      ;; install (gnu-build-system's default phases share #:make-flags).
      #:make-flags #~(list "dist_man_MANS=")
      ;; The test suite needs python3/pytest AND a live postgres server to
      ;; connect to -- inappropriate for (and unreachable from) the build
      ;; sandbox.
      #:tests? #f))
    (inputs (list libevent openssl))
    (native-inputs (list pkg-config))
    (synopsis "Lightweight connection pooler for PostgreSQL")
    (description
     "PgBouncer sits between PostgreSQL clients and the server, pooling and
reusing backend connections instead of opening a new one per client.  It
supports session, transaction, and statement pooling modes and can pool many
more client connections than @code{max_connections} allows on the server.")
    (home-page "https://www.pgbouncer.org/")
    (license license:isc)))
