(define-module (x-files packages yandex)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (nonguix licenses)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download))

(define hashes
  `(("amd64" . "120kv20v2a4i7mxlwy3281abfjfsdf0mn2q979s0lhhidk0jxmnc")
    ("386"   . "0h39bgnc1ghn740cdkkci8ay2qdngflmhsvijgvnw2wr8wipng8y")
    ("arm64" . "1inax1jqlhxnj2hyxnpg88ynk7wc74nm5c7ic8ijviz0xbpmxd80")))

(define target->arch
  `(("x86_64-linux"  . "amd64")
    ("i686-linux"    . "386")
    ("aarch64-linux" . "arm64")))

(define-public yandex-cloud-cli
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->arch target))]
    (package
      (name "yandex-cloud-cli")
      (version "0.199.0")
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
