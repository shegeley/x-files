(define-module (x-files packages go2rtc)
  #:use-module ((guix packages) #:select (package origin base32
                                           %current-system
                                           %current-target-system))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:export (go2rtc))

;; go2rtc — AlexxIT/go2rtc, an RTSP/WebRTC/HomeKit/HLS/MSE camera streaming
;; server used as the streaming backend behind Frigate and Home Assistant's
;; camera integration.  Upstream ships statically-linked Go binaries as bare
;; release assets (no archive) for several targets, so like xray-checker/glab
;; we just unpack -- no patchelf / rpath surgery needed.  Only x86_64-linux
;; and aarch64-linux are wired up here (mirroring officecli.scm's
;; target->bin-name/target->hash pattern); go2rtc_linux_arm (32-bit armhf) is
;; also published upstream and can be added the same way if ever needed.
;;
;; To bump: set version and recompute each hash with
;;   guix download https://github.com/AlexxIT/go2rtc/releases/download/vX.Y.Z/go2rtc_linux_amd64
;;   guix download https://github.com/AlexxIT/go2rtc/releases/download/vX.Y.Z/go2rtc_linux_arm64
(define target->bin-name
  '(("x86_64-linux"  . "go2rtc_linux_amd64")
    ("aarch64-linux" . "go2rtc_linux_arm64")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "19hm8qa26qxprxlw9vxz741y757v9jwjiqzxixkk3mvb4apidmij")
    ("aarch64-linux" . "0l2bj1qsmsilfmbpjcfnw6jn85zq1rmxzrjzln0ix9d7x2nsp7rm")))

(define-public go2rtc
  (let* [(target      (or (%current-target-system) (%current-system)))
         (go2rtc.bin  (assoc-ref target->bin-name target))
         (hash        (assoc-ref target->hash target))
         (version     "1.9.14")
         (uri         (string-append
                       "https://github.com/AlexxIT/go2rtc/releases/download/"
                       "v" version
                       "/" go2rtc.bin))]
    (package
      (name "go2rtc")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (file-name "go2rtc")
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        ;; bare binary asset, no tarball to unpack -- just install it.
        #:install-plan #~'(("go2rtc" "/bin/go2rtc"))
        ;; already a stripped static Go binary; re-stripping is pointless.
        #:strip-binaries? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _
                (chmod "go2rtc" #o755))))))
      (supported-systems targets)
      (home-page "https://github.com/AlexxIT/go2rtc")
      (synopsis "RTSP/WebRTC/HomeKit/HLS camera streaming server")
      (description
       "@code{go2rtc} is a real-time media streaming server and camera
gateway.  It converts between RTSP, RTMP, WebRTC, HomeKit, HLS/MSE, FFmpeg
and other sources/sinks, and serves as the streaming backend behind Frigate
NVR and Home Assistant's camera integration.  This package installs the
upstream statically-linked release binary for the target architecture
(x86_64 or aarch64).")
      (license license:expat))))
