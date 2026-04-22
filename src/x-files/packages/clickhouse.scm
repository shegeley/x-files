(define-module (x-files packages clickhouse)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)

  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)               #:select (package
                                                        origin
                                                        base32))
  #:use-module ((guix download)               #:select (url-fetch))
  #:use-module ((guix utils)                  #:select (%current-system
                                                        %current-target-system))

  #:use-module ((nonguix build-system binary) #:select (binary-build-system)))

(define %clickhouse-version "25.8.22.28")
(define %clickhouse-release-tag "v25.8.22.28-lts")

(define (clickhouse-url file)
  (string-append
   "https://github.com/ClickHouse/ClickHouse/releases/download/"
   %clickhouse-release-tag "/" file))

(define (clickhouse-extra-source component)
  "Return an origin for the COMPONENT ('server or 'client) extras tarball."
  (let* ((arch (match (or (%current-target-system) (%current-system))
                 ("aarch64-linux" "arm64")
                 (_ "amd64")))
         (hash (match (list component
                            (or (%current-target-system) (%current-system)))
                 (('server "aarch64-linux")
                  "18pwk4h5mgzfsgvi3hxqvccq6f2r17h3sdcp4d0bh12nq9pw3dwl")
                 (('server _)
                  "0dd66ld1w2bsna7w8f7z6lnn7l7z6v4hsibm9zybzabzv9iy1yi0")
                 (('client "aarch64-linux")
                  "0j11nl1i3s494kl44gk0n3k8ibgqr0rkx4wx5ss5pq9ra49bs85x")
                 (('client _)
                  "08a47sc1kg9n1xl8whi1q8iw4lrlpfwqlb7viwbxrf604d2k8l4g"))))
    (origin
      (method url-fetch)
      (uri (clickhouse-url
            (string-append "clickhouse-" (symbol->string component)
                           "-" %clickhouse-version "-" arch ".tgz")))
      (sha256 (base32 hash)))))

(define-public clickhouse-bin
  (package
    (name "clickhouse-bin")
    (version %clickhouse-version)
    (source
     (let* ((arch (match (or (%current-target-system) (%current-system))
                    ("aarch64-linux" "arm64")
                    (_ "amd64")))
            (hash (match (or (%current-target-system) (%current-system))
                    ("aarch64-linux"
                     "0j9r2qjwsrg373ikxmf9qkivymq51w3zcfkmfw57mcgdx39l1ya1")
                    (_
                     "1xgivnhdvir7wgwhgc27wbwjpvni4sgx01f9ra7xgw8anafi714h"))))
       (origin
         (method url-fetch)
         (uri (clickhouse-url
               (string-append "clickhouse-common-static-" version "-" arch ".tgz")))
         (sha256 (base32 hash)))))
    (build-system binary-build-system)
    (arguments
     (let ((server-src (clickhouse-extra-source 'server))
           (client-src (clickhouse-extra-source 'client)))
       (list
        #:substitutable? #f
        #:validate-runpath? #f
        #:patchelf-plan
        #~`(("usr/bin/clickhouse"                     ())
            ("usr/bin/clickhouse-extract-from-config" ()))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "tar" "-xvf" source)
                (invoke "tar" "-xvf" #$server-src)
                (invoke "tar" "-xvf" #$client-src)
                (chdir (string-append "clickhouse-common-static-" #$version))))
            (replace 'install
              (lambda _
                (let* ((out    #$output)
                       (bin    (string-append out "/bin"))
                       (etc    (string-append out "/etc"))
                       (share  (string-append out "/share"))
                       (srvdir (string-append "../clickhouse-server-" #$version))
                       (clidir (string-append "../clickhouse-client-" #$version)))
                  ;; Directories
                  (mkdir-p bin)
                  (mkdir-p (string-append etc "/clickhouse-server"))
                  (mkdir-p (string-append etc "/clickhouse-client"))
                  ;; Main binary + config extractor
                  (copy-file "usr/bin/clickhouse"
                             (string-append bin "/clickhouse"))
                  (copy-file "usr/bin/clickhouse-extract-from-config"
                             (string-append bin "/clickhouse-extract-from-config"))
                  (chmod (string-append bin "/clickhouse") #o755)
                  (chmod (string-append bin "/clickhouse-extract-from-config") #o755)
                  ;; Server symlinks
                  (for-each
                   (lambda (name)
                     (symlink "clickhouse" (string-append bin "/" name)))
                   '("clickhouse-server"
                     "clickhouse-keeper"
                     "clickhouse-keeper-client"
                     "clickhouse-keeper-converter"))
                  ;; Client symlinks
                  (for-each
                   (lambda (name)
                     (symlink "clickhouse" (string-append bin "/" name)))
                   '("clickhouse-client"
                     "clickhouse-local"
                     "clickhouse-benchmark"
                     "clickhouse-format"
                     "clickhouse-obfuscator"
                     "clickhouse-compressor"
                     "ch" "chc" "chl"))
                  ;; Protobuf schemas and bash completions
                  (copy-recursively "usr/share/clickhouse"
                                    (string-append share "/clickhouse"))
                  (copy-recursively "usr/share/bash-completion"
                                    (string-append share "/bash-completion"))
                  ;; Server configs
                  (copy-file
                   (string-append srvdir "/etc/clickhouse-server/config.xml")
                   (string-append etc "/clickhouse-server/config.xml"))
                  (copy-file
                   (string-append srvdir "/etc/clickhouse-server/users.xml")
                   (string-append etc "/clickhouse-server/users.xml"))
                  ;; Client config
                  (copy-file
                   (string-append clidir "/etc/clickhouse-client/config.xml")
                   (string-append etc "/clickhouse-client/config.xml")))))))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://clickhouse.com")
    (synopsis "Fast open-source column-oriented database management system")
    (description
     "ClickHouse is an open-source column-oriented database management system
that allows generating analytical data reports in real-time.  It is designed
to process analytical queries at high speed by storing data in a columnar
format.

This package ships the unified @code{clickhouse} binary along with
@code{clickhouse-server}, @code{clickhouse-client}, @code{clickhouse-local},
and other tools as symlinks, plus default configuration templates.")
    (license license:asl2.0)))
