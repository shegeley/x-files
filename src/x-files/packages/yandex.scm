(define-module (x-files packages yandex)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (nonguix licenses)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download))

(define hashes
  `(("amd64" . "0mkrsby5wxmxlw4jb1f7q846n59hnbcclmw15anc2gsmn65c2cbj")
    ("386"   . "175vklwhfh7kjms85n6g6r86gzrnhdwnm3hsjj04c8ybbx8953my")
    ("arm64" . "0l3nfn2g8iq8qkn4xi2c419hr9z6vg5mx9bbvy73npfcy9zhzadx")))

(define target->arch
  `(("x86_64-linux"  . "amd64")
    ("i686-linux"    . "386")
    ("aarch64-linux" . "arm64")))

(define-public yandex-cloud-cli
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->arch target))]
    (package
      (name "yandex-cloud-cli")
      (version "1.41.0")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://storage.yandexcloud.net/yandexcloud-yc/release/"
               version "/linux/" arch "/yc"))
         (sha256 (base32 (assoc-ref hashes arch)))))
      (build-system binary-build-system)
      (arguments
       (list
        #:install-plan #~'(("yc" "/bin/"))
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'chmod
                       (lambda _ (chmod "yc" #o755))))))
      (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))
      (synopsis "Yandex Cloud CLI interface")
      (description "YC provides downloadable software for managing your cloud resources from the command line")
      (home-page "https://cloud.yandex.com/en-ru/docs/cli/quickstart")
      (license (nonfree "https://yandex.cloud/en/docs/cli")))))
