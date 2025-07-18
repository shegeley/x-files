(define-module (x-files packages minio)
 #:use-module (guix build-system gnu)
 #:use-module (guix download)
 #:use-module (guix gexp)
 #:use-module (guix build-system copy)
 #:use-module (guix packages)
 #:use-module ((guix licenses) #:prefix license:))

(define-public minio-server
 (let* [(tag "RELEASE.2025-06-13T11-33-47Z")
        (binary-filename (string-append "minio." tag))
        (hash "1c545jiazkjbiycgy5fgv1hbylgp823wi7573m46ma2d6fh3z3b6")]
  (package
   (name "minio-server")
   (version tag)
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://dl.min.io/server/minio/release/linux-amd64/"
                  binary-filename))
            (sha256 (base32 hash))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan #~`(("minio" "/bin/minio"))
     #:phases #~(modify-phases %standard-phases
                 (replace 'unpack
                  (lambda* (#:key source #:allow-other-keys)
                   (copy-file source "minio")
                   (chmod "minio" #o755))))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://min.io")
   (synopsis "High-performance, S3 compatible object store")
   (description "Designed for speed and scalability, MinIO powers AI/ML, analytics, and data-intensive workloads with industry-leading performance")
   (license license:agpl3+))))

(define-public minio-client
 (let* [(tag "RELEASE.2025-05-21T01-59-54Z")
        (binary-filename (string-append "mc." tag))
        (hash "092dx74xalmk5x3585l3xs5yjcvrg8k6q4p1iligp0fpm51ca4gv")]
  (package
   (name "minio-client")
   (version tag)
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://dl.min.io/client/mc/release/linux-amd64/"
                  binary-filename))
            (sha256 (base32 hash))))
   (build-system copy-build-system)
   (arguments
    (list
     #:install-plan #~`(("mc" "/bin/mc"))
     #:phases #~(modify-phases %standard-phases
                 (replace 'unpack
                  (lambda* (#:key source #:allow-other-keys)
                   (copy-file source "mc")
                   (chmod "mc" #o755))))))
   (supported-systems '("x86_64-linux"))
   (home-page "https://min.io")
   (synopsis "MinIO Client")
   (description "MinIO Client")
   (license license:agpl3+))))
