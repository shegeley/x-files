(define-module (x-files packages electron)
  #:use-module ((nongnu packages electron)  #:select (electron-36))
  #:use-module ((guix packages)             #:select (package
                                                      origin
                                                      base32))
  #:use-module ((guix download)             #:select (url-fetch/zipbomb)))

;; (x-files packages kadr) pins this exact version in its
;; package-lock.json.  nongnu's own (nongnu packages electron) tops out at
;; electron-36; every major bump there from 27..36 has been a mechanical
;; (inherit ...) + new version/source with no chromium-binary-build-system
;; or install-plan changes (confirmed via nonguix's own git history for
;; electron.scm), so the same mechanical bump is applied here for 42.
(define-public electron-42
  (package
    (inherit electron-36)
    (version "42.4.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/electron/electron/releases/download/v"
                           version "/electron-v" version "-linux-x64.zip"))
       (sha256
        (base32 "195xpiib5a6m0qjlgr6y6ilfb2kk2sqw5i4w15b0lja8amir90cs"))))))
