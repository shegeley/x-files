(define-module (x-files packages wireguard)
 #:use-module (guix gexp)
 #:use-module (guix download)
 #:use-module (guix packages)
 #:use-module (guix build-system copy)

 #:use-module (ice-9 match)

 #:use-module (nonguix licenses))

(define-public wg-fake
 ;; TODO: repack as "gnu-build-system" package
 (let* [(target (or (%current-target-system) (%current-system)))
        (wg-fake.bin-name
         (string-append "wg-fake.linux-"
          (match target
           ("x86_64-linux"  "amd64")
           ("i686-linux"    "386")
           ("armv7-linux"   "arm")
           ("aarch64-linux" "arm64"))))
        (hash
         (match target
          ("x86_64-linux"  "18axl9i504mm6h013lmfragn696nkrd7yfy30alkdnjvgz3qd3yf")
          ("i686-linux"    "0m23gwlwfbjiq7yb45s97dscgdlkil725x8kifawiv18zd5rwqsg")
          ("armv7-linux"   "1qywfhm0kmq05wj2v7yzgvg939jzgky854cp9gg7qhvgxaqwvfrj")
          ("aarch64-linux" "07q3rarb9ni2qfpjd2w9h9bi5rjz30gdj2pm7xf6w5f2pg8xdkhi")))]
  (package
   (name "wg-fake")
   (version "1.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/lastbyte32/wg-fake/releases/download/v" version "/" wg-fake.bin-name))
     (sha256 (base32 hash))))
   (build-system copy-build-system)
   (arguments (list
               #:phases #~(modify-phases %standard-phases
                           (add-after 'unpack 'chmod
                            (lambda _
                             (chmod '#$wg-fake.bin-name #o755))))
               #:install-plan #~'((#$wg-fake.bin-name "/bin/wg-fake"))))
   (synopsis "DPI-bypass for Wireguard handshake")
   (home-page "https://github.com/lastbyte32/wg-fake")
   (description "Fake handshake for WireGuard. Allows to bypass DPI blocking of the WireGuard protocol by sending a \"magic\" packet")
   (license (nonfree (string-append "None" "To be clarified"))))))
