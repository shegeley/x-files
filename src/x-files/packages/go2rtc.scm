(define-module (x-files packages go2rtc)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:export (go2rtc))

;; go2rtc — AlexxIT/go2rtc, an RTSP/WebRTC/HomeKit/HLS/MSE camera streaming
;; server used as the streaming backend behind Frigate and Home Assistant's
;; camera integration.  Upstream ships a statically-linked linux amd64 Go
;; binary as a bare release asset (no archive), so like xray-checker/glab we
;; just unpack -- no patchelf / rpath surgery needed.
;;
;; To bump: set %version and recompute the hash with
;;   guix download https://github.com/AlexxIT/go2rtc/releases/download/vX.Y.Z/go2rtc_linux_amd64
(define %version "1.9.14")

(define-public go2rtc
  (package
    (name "go2rtc")
    (version %version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/AlexxIT/go2rtc/releases/download/v"
             %version "/go2rtc_linux_amd64"))
       (file-name "go2rtc")
       (sha256
        (base32 "19hm8qa26qxprxlw9vxz741y757v9jwjiqzxixkk3mvb4apidmij"))))
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
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/AlexxIT/go2rtc")
    (synopsis "RTSP/WebRTC/HomeKit/HLS camera streaming server")
    (description
     "@code{go2rtc} is a real-time media streaming server and camera
gateway.  It converts between RTSP, RTMP, WebRTC, HomeKit, HLS/MSE, FFmpeg
and other sources/sinks, and serves as the streaming backend behind Frigate
NVR and Home Assistant's camera integration.  This package installs the
upstream statically-linked linux amd64 release binary.")
    (license license:expat)))
