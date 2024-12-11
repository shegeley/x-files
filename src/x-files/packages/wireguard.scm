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
           ("aarch64-linux" "arm64"))))]
  (package
   (name "wg-fake")
   (version "0.0.1")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/lastbyte32/wg-fake/releases/download/v" version "/" wg-fake.bin-name))
     (sha256 (base32 "1zlp9zvwfslbs8iannxn5qc6lvlswmkc6xmn5w66van7vv6ynyj4"))))
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
