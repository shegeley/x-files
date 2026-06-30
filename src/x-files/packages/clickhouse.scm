(define-module (x-files packages clickhouse)
  #:use-module (guix gexp)

  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)               #:select (package
                                                        origin
                                                        base32))
  #:use-module ((guix download)               #:select (url-fetch))
  #:use-module ((guix utils)                  #:select (%current-system
                                                        %current-target-system))

  #:use-module ((nonguix build-system binary) #:select (binary-build-system)))

(define %clickhouse-version "26.3.12.3")
(define %clickhouse-release-tag "v26.3.12.3-lts")

(define (clickhouse-url file)
  (string-append
   "https://github.com/ClickHouse/ClickHouse/releases/download/"
   %clickhouse-release-tag "/" file))

;; Per-system download arch (default amd64) and per-(component,system) sha256.
;; component is one of 'common (the main binary), 'server, 'client.  The "_"
;; key is the fallback used for any non-aarch64 system (i.e. x86_64).
(define %arch
  '(("aarch64-linux" . "arm64")
    ("_"             . "amd64")))

(define %hashes
  '((common ("aarch64-linux" . "1a9v57c0x9ndfw641j9q3m7pb951ipsam57mx1fpqjnlsjaw46p2")
            ("_"             . "1mpa8ns2msx7yf9r7685zlwzassh9pmymcv4jh7niygn8vbyxfkx"))
    (server ("aarch64-linux" . "1x7n4gw9ihipgx18j34aqh2qr4h2knlqhw8gx6xxpih6f93sxv8z")
            ("_"             . "0ddq7a2dxzkrl5kz9an514ymgv1d9pnsxswdzinzk6aly52dzmcp"))
    (client ("aarch64-linux" . "00mwr2dsqw8y9j2vhjlnfqr3vd0n3m7m671bdrqijzaqlr2gv5l9")
            ("_"             . "05wngzkz5dgv7kll0wp4l1c6ar72xicbf40slyxz99y0a1mz7dy3"))))

(define (current-system*) (or (%current-target-system) (%current-system)))

(define (lookup alist key)
  "assoc-ref ALIST by KEY, falling back to the \"_\" default entry."
  (or (assoc-ref alist key) (assoc-ref alist "_")))

(define (system-arch)        (lookup %arch (current-system*)))
(define (component-hash component)
  (lookup (assoc-ref %hashes component) (current-system*)))

(define (clickhouse-origin component file-prefix)
  "Origin for the COMPONENT ('common/'server/'client) tarball named
FILE-PREFIX-<version>-<arch>.tgz."
  (origin
    (method url-fetch)
    (uri (clickhouse-url
          (string-append file-prefix "-" %clickhouse-version
                         "-" (system-arch) ".tgz")))
    (sha256 (base32 (component-hash component)))))

(define (clickhouse-extra-source component)
  "Return an origin for the COMPONENT ('server or 'client) extras tarball."
  (clickhouse-origin component
                     (string-append "clickhouse-" (symbol->string component))))

(define-public clickhouse-bin
  (package
    (name "clickhouse-bin")
    (version %clickhouse-version)
    (source (clickhouse-origin 'common "clickhouse-common-static"))
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
