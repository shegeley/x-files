(define-module (x-files packages cups drivers canon)
  #:use-module (x-files utils base)

  #:use-module (guix licenses)
  #:use-module (gnu services cups)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)

  #:use-module (gnu packages)
  #:use-module (guix packages)

  #:use-module (guix build union)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (guix build copy-build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)

  #:use-module (gnu packages gnome)

  #:use-module (oop goops)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)

  #:export (canon-eula
            samsung-drivers))

(define canon-eula
   ((@@ (guix licenses) license) "Canon EULA"
   "https://global.canon"
   "End-user license agreement for Canon electronics software product"))

(define cndrvcups-lb
  ;; see https://github.com/NixOS/nixpkgs/blob/f94467a5c94335fc68a54f6bb12b85c00ecc7821/pkgs/misc/cups/drivers/canon/default.nix#L31
  (package
    (name "cndrvcups-lb")
    (version "5.80-1.05")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gdlp01.c-wss.com/gds/8/0100007658/39/linux-UFRII-drv-v580-m17n-05.tar.gz"))
       (sha256
        (base32 "0lqw3qzrnrg2xcg6lxlpqh2bw3bm6hjd51w6yz8hp5m8h24hyxja"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-build? #f
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'extract
                     (lambda _
                       (begin
                         (use-modules (guix build utils))
                         (chdir "Sources")
                         (invoke #$(file-append tar "/bin/tar") "xvf"
                                 "cnrdrvcups-lb-5.80-1.05.tar.xz"
                                 "--one-top-level")
                         (chdir "cnrdrvcups-lb-5.80-1.05/cnrdrvcups-lb-5.80"))))
                   (replace 'configure
                     (lambda _
                       (begin
                         ;; delete the last line in allgen (call to 'make')
                         (invoke #$(file-append sed "/bin/sed") "--in-place" "47d" "allgen.sh")
                         (invoke "bash" "./allgen.sh")))))))
    (inputs
     (list coreutils sed ghostscript foomatic-filters
           gnu-gettext automake libtool autoconf))
    (supported-systems '("x86_64-linux"))
    (home-page "https://canon.com")
    (synopsis "Canon UFR II/UFRII LT Printer Driver for Linux")
    (description "This printer driver provides printing functions for UFRII-enabled Canon printers operating under the CUPS (Common UNIX Printing System) environment, a printing system that operates on Linux operating systems. To use this software, please read the online manual before installing the driver.")
    (license (list canon-eula))))

cndrvcups-lb
