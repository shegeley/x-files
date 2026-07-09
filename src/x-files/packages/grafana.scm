(define-module (x-files packages grafana)
  #:use-module ((guix licenses)          #:prefix license:)
  #:use-module ((guix packages)          #:select (package origin base32))
  #:use-module ((guix download)          #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix build-system copy) #:select (copy-build-system))
  #:use-module ((guix utils)             #:select (%current-system %current-target-system))
  #:use-module (ice-9 match))

(define-public grafana-bin
  (package
    (name "grafana-bin")
    (version "13.1.0")
    (source
     (let* ((arch (match (or (%current-target-system) (%current-system))
                    ("aarch64-linux" "arm64")
                    (_ "amd64")))
            (hash (match (or (%current-target-system) (%current-system))
                    ("aarch64-linux"
                     "16iqdd1mcazjm3wyk6hpvzjmls4lngkgvm906cbpp49bg42q7yfm")
                    (_
                     "0xg78pliiqgh5x7p33fshwbb8k18pfx834vq8y5pbfxq4jr2nmjg"))))
       (origin
         (method url-fetch)
         (uri (string-append
               "https://dl.grafana.com/oss/release/grafana-" version
               ".linux-" arch ".tar.gz"))
         (sha256 (base32 hash)))))
    (build-system copy-build-system)
    (arguments
     (list
      #:substitutable? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "-xvf" source)
              (chdir (string-append "grafana-" #$version))))
          (replace 'install
            (lambda _
              (let ((bin   (string-append #$output "/bin"))
                    (share (string-append #$output "/share/grafana")))
                (mkdir-p bin)
                (for-each
                 (lambda (dir)
                   (copy-recursively dir (string-append share "/" dir)))
                 '("conf" "public" "plugins-bundled" "data" "tools"))
                (copy-file "bin/grafana"
                           (string-append bin "/grafana"))
                (chmod (string-append bin "/grafana") #o755)))))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (home-page "https://grafana.com")
    (synopsis "Open-source analytics and monitoring solution")
    (description
     "Grafana is an open-source platform for monitoring and observability.
It allows you to query, visualize, alert on, and explore your metrics,
logs, and traces no matter where they are stored.  Grafana provides
tools to turn your time-series database data into insightful graphs
and visualizations.

The binary is the unified @code{grafana} executable introduced in
Grafana 10, which combines the former @code{grafana-server} and
@code{grafana-cli} entry points.  Start the server with:
@code{grafana server --homepath /path/to/grafana/share/grafana}")
    (license license:agpl3+)))
