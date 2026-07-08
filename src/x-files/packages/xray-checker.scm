(define-module (x-files packages xray-checker)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:export (xray-checker))

;; xray-checker — kutovoys/xray-checker, a monitor for the availability and
;; quality of Xray/V2Ray/etc. proxy nodes: it dials through each subscription
;; config, measures latency + connectivity, and exposes Prometheus metrics and
;; a web status UI.  Upstream ships a statically-linked linux amd64 Go binary
;; (`file` reports "statically linked"), so — like glab — we just unpack the
;; release tarball; no patchelf / rpath surgery needed.
;;
;; To bump: set %version and recompute the hash with
;;   guix download https://github.com/kutovoys/xray-checker/releases/download/vX.Y.Z/xray-checker-vX.Y.Z-linux-amd64.tar.gz
(define %version "1.3.1")

(define-public xray-checker
  (package
    (name "xray-checker")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/kutovoys/xray-checker/releases/download/v"
             %version "/xray-checker-v" %version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "1z7wb9xfvxc1ai2md037lqalfgfwd4msaw8n3klpwvv3hl37y9nn"))))
    (build-system binary-build-system)
    (arguments
     (list
      ;; tarball layout: xray-checker (binary) + README.md, both top-level.
      #:install-plan #~'(("xray-checker" "/bin/xray-checker"))
      ;; already a stripped static Go binary; re-stripping is pointless.
      #:strip-binaries? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; The stock 'unpack chdirs into the first subdirectory it finds; this
          ;; tarball has no subdir (files at the top), so extract and stay at the
          ;; top to keep the install-plan's paths stable.
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "tar" "--extract" "--file" source)))
          (add-after 'unpack 'chmod
            (lambda _
              (chmod "xray-checker" #o755))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/kutovoys/xray-checker")
    (synopsis "Availability + quality monitor for Xray/VLESS proxy nodes")
    (description
     "@code{xray-checker} probes proxy endpoints defined in Xray, V2Ray,
Sing-box, and Clash subscription configs — dialing through each node to measure
latency and connectivity — and exposes the results as Prometheus metrics and a
web status page.  This package installs the upstream statically-linked linux
amd64 release binary.")
    (license license:expat)))
