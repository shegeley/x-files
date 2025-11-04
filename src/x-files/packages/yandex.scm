(define-module (x-files packages yandex)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (nonguix licenses)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download))

(define hashes
  `(("amd64" . "1fxkvp7d42kcs79ms8vzh31bmac5hapmz74hz86jba2s7my8bw0n")
    ("386"   . "1j9ilrwrmdzmai2f27ma1hnaak9fkx3ah1988f8bl42ddfx3b9lq")
    ("arm64" . "0i3812lgxdh247cfyiv34ijcfpkrw1f7242rv7f8kykd97229bv4")))

(define target->arch
  `(("x86_64-linux"  . "amd64")
    ("i686-linux"    . "386")
    ("aarch64-linux" . "arm64")))

(define-public yandex-cloud-cli
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->arch target))]
    (package
      (name "yandex-cloud-cli")
      (version "0.173.0")
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
