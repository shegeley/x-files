(define-module (x-files packages yandex)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (nonguix licenses)
  #:use-module (guix gexp)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download))

(define hashes
  `(("amd64" . "0pn3q8nmr97shxj4dxyczdcrpbr39padj1x0d6f75q6ppknplyf8")
    ("386" . "1x7jyvjx0q3x018yyy0cbfshsncw98743j802mxm90pf9rahf56r")
    ("arm64" . "0y2jyfsr9b39y1hq2r6ada81d0ky641qydsspbphz1sxv3bmzyrd")))

(define-public yandex-cloud-cli
  ;; https://yandex.cloud/en/docs/cli/release-notes#version0i.146.1
  (let* [(arch-match (lambda (arch)
                       (match arch
                         ((or "x86_64-linux") "amd64")
                         ((or "i686-linux") "386")
                         ((or "aarch64-linux") "arm64"))))
         (arch (arch-match (or (%current-target-system)
                               (%current-system))))]
    (package
      (name "yandex-cloud-cli")
      (version "0.146.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://storage.yandexcloud.net/yandexcloud-yc/release/"
               version "/linux/" arch "/yc"))
         (sha256 (base32 (assoc-ref hashes arch)))))
      (build-system binary-build-system)
      (arguments
       (list #:install-plan
             #~'(("yc" "/bin/"))
                #:phases
                #~(modify-phases %standard-phases
                    (add-after 'unpack 'chmod
                      (lambda _ (chmod "yc" #o755))))))
      (inputs `())
      (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))
      (synopsis "Yandex Cloud CLI interface")
      (description "YC provides downloadable software for managing your cloud resources from the command line")
      (home-page "https://cloud.yandex.com/en-ru/docs/cli/quickstart")
      (license (nonfree "https://yandex.cloud/en/docs/cli")))))
