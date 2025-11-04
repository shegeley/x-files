(define-module (x-files packages minio)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define minio-server-tag "RELEASE.2025-09-07T16-13-09Z")

(define target->minio-target
  '(("x86_64-linux"  . "linux-amd64")
    ("aarch64-linux" . "linux-arm64")))

(define targets (map car target->minio-target))

(define target->minio-server-hash
  '(("x86_64-linux"  . "0pw5zdji0a3k81y3gaw3gb3r3ldjb29j166in5an95kf5i8xhnvw")
    ("aarch64-linux" . "0gghsr7nhz8zgzfln5375cknxwrgh33y2wrz4jh7nwaiy4ncv0sw")))

(define-public minio-server
  (let* [(tag    minio-server-tag)
         (target (or (%current-target-system) (%current-system)))
         (minio-target (assoc-ref target->minio-target target))
         (binary-filename (string-append "minio." minio-server-tag))
         (hash (assoc-ref target->minio-server-hash target))]
    (package
      (name "minio-server")
      (version tag)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://dl.min.io/server/"
                      "minio/release/" minio-target
                      "/" binary-filename))
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
      (supported-systems targets)
      (home-page "https://min.io")
      (synopsis "High-performance, S3 compatible object store")
      (description "Designed for speed and scalability, MinIO powers AI/ML, analytics, and data-intensive workloads with industry-leading performance")
      (license license:agpl3+))))

(define minio-client-tag "RELEASE.2025-08-13T08-35-41Z")

(define target->minio-client-hash
  '(("x86_64-linux"  . "14g8281w1419nf5rm0id4h3b35b6q3bsavqi14mprf7rqplndy01")
    ("aarch64-linux" . "0b614x61vl6sx70549rf7cwdxs2468sh84qnvmm67r7wdihwkj0l")))

(define-public minio-client
  (let* [(tag minio-client-tag)
         (target (or (%current-target-system) (%current-system)))
         (minio-target (assoc-ref target->minio-target target))
         (binary-filename (string-append "mc." tag))
         (hash (assoc-ref target->minio-client-hash target))]
    (package
      (name "minio-client")
      (version tag)
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://dl.min.io/client/"
                      "mc/release/" minio-target
                      "/" binary-filename))
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
      (supported-systems targets)
      (home-page "https://min.io")
      (synopsis "MinIO Client")
      (description "MinIO Client")
      (license license:agpl3+))))
