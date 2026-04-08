(define-module (x-files packages gql)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix build-system binary)
  #:use-module (guix gexp))

(define target->arch-name
  '(("x86_64-linux"  . "x86_64")
    ("aarch64-linux" . "aarch64")))

(define target->hash
  '(("x86_64-linux"  . "0kpklygs8b96lpyijamf2snp4j4n78svml89b5c5dxk7yyzp9hlx")
    ("aarch64-linux" . "1jrwchlnfvnxb5145gpv25zabzfm90nrlzdqrbci20jkgbv57w2k")))

(define-public gql
  (let* [(target  (or (%current-target-system) (%current-system)))
         (arch    (assoc-ref target->arch-name target))
         (hash    (assoc-ref target->hash target))
         (version "0.43.0")]
    (package
      (name "gql")
      (version version)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/AmrDeveloper/GQL/releases/download/"
                      version "/gql-" arch "-linux.gz"))
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:validate-runpath? #f
        #:install-plan ''(("./gql" "/bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (copy-file source "gql.gz")
                (invoke "gzip" "-d" "gql.gz")
                (chmod "gql" #o755))))))
      (supported-systems (map car target->arch-name))
      (home-page "https://github.com/AmrDeveloper/GQL")
      (synopsis "SQL-like language to perform queries on Git repositories")
      (description "GQL (Git Query Language) is a SQL-like language to perform
queries on .git files with support for most SQL features such as grouping,
ordering, and aggregation functions.")
      (license license:expat))))
