(define-module (x-files android twrp)
 #:use-module (guix packages)
 #:use-module (guix download)
 #:use-module (gnu packages compression)
 #:use-module (guix build-system copy)
 #:use-module (nonguix licenses))

(define-public twrp-surya
 (package
  (name "twrp-surya")
  (version "20240126-1820")
  (native-inputs (list unzip))
  (source
   (origin
    (method url-fetch)
    (uri (string-append "https://github.com/brigudav/android_device_xiaomi_surya_twrp/releases/download/7664715524/twrp-surya-" version ".zip"))
    (sha256 (base32 "16fjy0c23skdp1r18zrq00nzs2xzm7vpgs6gl6xdancgcfxg7drl"))))
  (build-system copy-build-system)
  (synopsis "[Unofficial] Device Tree for building TWRP for POCO X3 NFC (karna/surya)")
  (home-page "https://github.com/brigudav/android_device_xiaomi_surya_twrp")
  (description "")
  (license (nonfree (string-append "?" "?")))))

(define-public twrp-mocha
 (package
  (name "twrp-mocha")
  (version "3.4.0-0")
  (source
   (origin
    (method url-fetch)
    (uri (string-append "https://eu.dl.twrp.me/mocha/twrp-" version "-mocha.img"))
    (sha256 (base32 "18b5p35qp2q4n5xnfqxjdzvrilkzw7jzj83sjl0qcf66flkmnx2p"))))
  (build-system copy-build-system)
  (synopsis "[Unofficial] Device Tree for building TWRP for Mi Pad 2 (mocha)")
  (home-page "https://eu.dl.twrp.me/mocha/twrp-3.4.0-0-mocha.img.html")
  (description "")
  (license (nonfree (string-append "?" "?")))))
