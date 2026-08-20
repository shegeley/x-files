(define-module (x-files packages atlasgo)
  #:use-module ((guix packages) #:select (package origin base32
                                           %current-system
                                           %current-target-system))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:export (atlasgo))

;; atlasgo — ariga/atlas, "schema as code" database migration tool
;; (atlasgo.io). Upstream's GitHub Releases page ships NO downloadable
;; assets; real binaries are only served from release.ariga.io (see
;; https://atlasgo.sh, the official install script). That server hosts TWO
;; flavors per platform: the DEFAULT binary (built under Ariga's own MSA --
;; not a free-software license) and a "community" binary (Apache 2.0). Only
;; the community flavor is packaged here. Bare binary asset, no archive --
;; same shape as go2rtc.scm/xray-checker.scm, no patchelf/rpath surgery
;; needed since it's a statically-linked Go binary.
;;
;; To bump: set version and recompute the hash with
;;   guix download https://release.ariga.io/atlas/atlas-community-linux-amd64-vX.Y.Z
;; (aarch64-linux: swap amd64 -> arm64, not wired up below since none of this
;; channel's consumers run non-x86_64-linux; add the same way go2rtc.scm does
;; if ever needed.)
(define target->bin-name
  '(("x86_64-linux" . "atlas-community-linux-amd64")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux" . "02y718xfh40xp7rsv4y2dnhi3bxdrfj385fpp2csnhyf7lz93mqh")))

(define-public atlasgo
  (let* [(target      (or (%current-target-system) (%current-system)))
         (atlas.bin   (assoc-ref target->bin-name target))
         (hash        (assoc-ref target->hash target))
         (version     "1.3.0")
         (uri         (string-append
                       "https://release.ariga.io/atlas/"
                       atlas.bin "-v" version))]
    (package
      (name "atlasgo")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (file-name "atlas")
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        ;; bare binary asset, no tarball to unpack -- just install it.
        #:install-plan #~'(("atlas" "/bin/atlas"))
        ;; already a stripped static Go binary; re-stripping is pointless.
        #:strip-binaries? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _
                (chmod "atlas" #o755))))))
      (supported-systems targets)
      (home-page "https://atlasgo.io")
      (synopsis "Manage database schemas as code, with auto-generated migrations")
      (description
       "@code{atlas} is a database schema management tool. It compares a
declared schema (HCL, SQL, or an ORM's model) against a live database and
generates -- or, in declarative mode, directly applies -- the migration
needed to reconcile them, Terraform-style. Supports PostgreSQL, MySQL,
MariaDB, SQL Server, SQLite, ClickHouse, and others. This package installs
the upstream statically-linked \"community\" edition release binary
(Apache 2.0 licensed; distinct from the default MSA-licensed build Ariga
also distributes) for x86_64-linux.")
      (license license:asl2.0))))
