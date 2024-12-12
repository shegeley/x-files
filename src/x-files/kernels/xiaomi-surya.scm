(define-module (x-files kernels sm7150)
  #:use-module (nongnu packages linux)
  #:use-module (guix download)

  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:use-module (nonguix licenses)

  #:use-module (gnu packages linux))

(define config->string (@@ (gnu packages linux) config->string))

(define xiaomi-surya-firmware
 (let ((commit    "899cc94e35510e7877162655f36f35558cb50642")
       (home-page "https://github.com/sm7150-mainline/firmware-xiaomi-surya")
       (hash      "10jcznjq3pnwwc6lbckmvq381gxp72fyl2y8w9c1ibi9ywnpdm86")
       (revision  "1"))
  (package
   (name "xiaomi-surya-firmware")
   (version (git-version "0.0.0" revision commit))
   (home-page home-page)
   (arguments (list #:strip-binaries? #f
               #:validate-runpath? #f))
   (source (origin
            (method git-fetch)
            (uri (git-reference (url home-page) (commit commit)))
            (file-name (git-file-name name version))
            (sha256 (base32 hash))))
   (build-system copy-build-system)
   (synopsis "Nonfree Linux firmware blobs for xiaomi-surya")
   (description "Nonfree Linux firmware blobs for xiaomi-surya")
   (license (nonfree (string-append "https://git.kernel.org/pub/scm/linux/kernel/git/" "firmware/linux-firmware.git/plain/WHENCE"))))))

(define xiaomi-surya
 (let [(url    "https://github.com/xiaomi-surya/android_kernel_xiaomi_surya")
       (commit "557eb775fff7aadeb8e9e7ca46aae7e4c91086d4")
       (hash   "0cpvlfl7yfbj01c90n9a7an38d1kjr1ji91w9ypl6k5ka4n4xl32")]
  (customize-linux
   #:name "xiaomi-surya"
   #:linux linux-arm64-generic
   #:defconfig (local-file "./xiaomi-surya.config") ;; TODO: fix
   #:extra-version "beta"
   #:configs (config->string '(("CONFIG_LOCALVERSION" . "")))
   #:source (origin
             (method git-fetch)
             (uri (git-reference (url url) (commit commit) (recursive? #t)))
             (sha256 (base32 hash))))))
