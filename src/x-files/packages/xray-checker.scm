(define-module (x-files packages xray-checker)
  #:use-module ((guix packages) #:select (package origin base32
                                           %current-system
                                           %current-target-system))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:export (xray-checker))

;; xray-checker — kutovoys/xray-checker, a monitor for the availability and
;; quality of Xray/V2Ray/etc. proxy nodes: it dials through each subscription
;; config, measures latency + connectivity, and exposes Prometheus metrics and
;; a web status UI.  Upstream ships statically-linked Go binaries (`file`
;; reports "statically linked") for several targets, so — like glab — we just
;; unpack the release tarball; no patchelf / rpath surgery needed.  Both
;; x86_64-linux and aarch64-linux tarballs share the same top-level layout
;; (README.md + the binary), so one install-plan covers both.
;;
;; To bump: set %version and recompute each hash with
;;   guix download https://github.com/kutovoys/xray-checker/releases/download/vX.Y.Z/xray-checker-vX.Y.Z-linux-amd64.tar.gz
;;   guix download https://github.com/kutovoys/xray-checker/releases/download/vX.Y.Z/xray-checker-vX.Y.Z-linux-arm64.tar.gz
(define %version "1.3.1")

(define target->arch
  '(("x86_64-linux"  . "amd64")
    ("aarch64-linux" . "arm64")))

(define targets (map car target->arch))

(define target->hash
  '(("x86_64-linux"  . "1z7wb9xfvxc1ai2md037lqalfgfwd4msaw8n3klpwvv3hl37y9nn")
    ("aarch64-linux" . "02jdhg60rkr4y7j0qnprr0vhky2yd3b20dzf0ggdar6g6rcdrih5")))

(define-public xray-checker
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->arch target))
         (hash   (assoc-ref target->hash target))]
    (package
      (name "xray-checker")
      (version %version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/kutovoys/xray-checker/releases/download/v"
               %version "/xray-checker-v" %version "-linux-" arch ".tar.gz"))
         (sha256
          (base32 hash))))
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
      (supported-systems targets)
      (home-page "https://github.com/kutovoys/xray-checker")
      (synopsis "Availability + quality monitor for Xray/VLESS proxy nodes")
      (description
       "@code{xray-checker} probes proxy endpoints defined in Xray, V2Ray,
Sing-box, and Clash subscription configs — dialing through each node to measure
latency and connectivity — and exposes the results as Prometheus metrics and a
web status page.  This package installs the upstream statically-linked
release binary for the target architecture (x86_64 or aarch64).")
      (license license:expat))))
