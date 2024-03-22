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
  (let* [(major-version "5.80")
         (minor-version "1.0.5")
         (full-version (string-append major-version "-" minor-version))
         (uri "https://gdlp01.c-wss.com/gds/8/0100007658/39/linux-UFRII-drv-v580-m17n-05.tar.gz")
         (hash "0lqw3qzrnrg2xcg6lxlpqh2bw3bm6hjd51w6yz8hp5m8h24hyxja")
         (dirname* (lambda (type) (string-append
                              "Sources/"
                              "cnrdrvcups"
                              "-" "lb" "-"
                              full-version
                              (if type
                                  (string-append
                                   "cnrdrvcups"
                                   "-" type "-"
                                   major-version)
                                  ""))))
         (dir:tar (string-append (dirname* #f) ".tar.xz"))
         (dir:lb (dirname* "lb"))
         (dir:common (dirname* "common"))
         (dir:utility (dirname* "utility"))]
    (package
      (name "cndrvcups")
      (version full-version)
      (source
       (origin
         (method url-fetch)
         (uri uri)
         (sha256 (base32 hash))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:parallel-build? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'extract
              (lambda _
                (begin
                  (use-modules (guix build utils))
                  (invoke #$(file-append tar "/bin/tar")
                          "xvf"
                          #$dir:tar
                          "--one-top-level"))))
            (add-before 'install 'replace-refs-in-common
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out")))
                  (substitute* (string-append #$dir:common "/" "allgen.sh")
                    (("^prefix = .*$")
                     (string-append "prefix = " out "\n"))
                    (("^libdir = .*$")
                     (string-append "prefix = " out "/lib" "\n"))
                    (("^bindir = .*$")
                     (string-append "prefix = " out "/bin" "\n")))
                  (substitute* (string-append #$dir:common "/" "backend" "/" "Makefile.am")
                    (("usr") out))
                  (substitute* (string-append #$dir:common "rasterfilter" "/" "cnrasterproc.h")
                    (("usr") out))
                  (substitute* (string-append #$dir:common "/" "cngplp" "/" "Makefile.am")
                    (("etc/cngplp") (string-append #$out "/etc/cngplp")))
                  (substitute* (string-append #$dir:common "/" "cngplp" "/"
                                              "src" "/" "Makefile.am")
                    (("usr/share/cngplp") (string-append #$out "/usr/share/cngplp")))
                  ;; nix has patchShebangs here. But they should be automatically pathed by guix
                  ;; https://github.com/NixOS/nixpkgs/blob/c2b7425324a3fa95e064216c503c2ac2e89abe57/pkgs/misc/cups/drivers/canon/default.nix#L61C6-L61C49
                  )))
            (add-afer 'replace-refs-in-common 'replace-refs-in-lb
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out")))
                  (substitute* (string-append #$dir:lb "/" "allgen.sh")
                    (("^prefix = .*$")
                     (string-append "prefix = " out "\n"))
                    (("^libdir = .*$")
                     (string-append "prefix = " out "/lib" "\n"))
                    (("^bindir = .*$")
                     (string-append "prefix = " out "/bin" "\n"))
                    ;; didn't get this one:
                    ;; sed -ie '/^cd \.\.\/cngplp/,/^cd files/{/^cd files/!{d}}' cnrdrvcups-lb-${version}/allgen.sh
                    (("cd \.\./pdftocpca") "cd pdftocpca")
                    (("/usr") out)
                    (("CNGPLPDIR") "")))))
            (delete 'configure)
            (replace 'install 'install-common
              (lambda _
                (begin
                  (chdir #$dir:common)
                  (invoke "bash" "/allgen.sh")
                  (invoke "make" "install"))))
            (add-after 'install-common 'fix-usb-quirks
              (lambda _
                #| TODO
                cd cnrdrvcups-common-${version}/Rule
                mkdir -p $out/share/cups/usb
                install -m 644 *.usb-quirks $out/share/cups/usb |#
                ))
            (add-after 'fix-usb-quirks 'install-lb
              (lambda _
                #| TODO
                cd cnrdrvcups-lb-${version}
                ./allgen.sh
                make install|#
                ))
            (add-after 'install-lb 'install-ppds
              (lambda _
                #| TODO
                 mkdir -p $out/share/cups/model
                install -m 644 ppd/*.ppd $out/share/cups/model/
                |#
                ))
            (add-after 'install-ppds 'install-libs-and-filters
              (lambda _
                #| TODO
                cd lib
                mkdir -p $out/lib
                install -m 755 libs64/${system}/libColorGearCufr2.so.2.0.0 $out/lib
                install -m 755 libs64/${system}/libcaepcmufr2.so.1.0 $out/lib
                install -m 755 libs64/${system}/libcaiocnpkbidir.so.1.0.0 $out/lib
                install -m 755 libs64/${system}/libcaiousb.so.1.0.0 $out/lib
                install -m 755 libs64/${system}/libcaiowrapufr2.so.1.0.0 $out/lib
                install -m 755 libs64/${system}/libcanon_slimufr2.so.1.0.0 $out/lib
                install -m 755 libs64/${system}/libcanonufr2r.so.1.0.0 $out/lib
                install -m 755 libs64/${system}/libcnaccm.so.1.0 $out/lib
                install -m 755 libs64/${system}/libcnlbcmr.so.1.0 $out/lib
                install -m 755 libs64/${system}/libcnncapcmr.so.1.0 $out/lib
                install -m 755 libs64/${system}/libufr2filterr.so.1.0.0 $out/lib

                install -m 755 libs64/${system}/cnpdfdrv $out/bin
                install -m 755 libs64/${system}/cnpkbidir $out/bin
                install -m 755 libs64/${system}/cnpkmoduleufr2r $out/bin
                install -m 755 libs64/${system}/cnrsdrvufr2 $out/bin
                install -m 755 libs64/${system}/cnsetuputil2 $out/bin/cnsetuputil2

                mkdir -p $out/share/cnpkbidir
                install -m 644 libs64/${system}/cnpkbidir_info* $out/share/cnpkbidir

                mkdir -p $out/share/ufr2filter
                install -m 644 libs64/${system}/ThLB* $out/share/ufr2filter
                |#
                ))
            (add-after 'install-libs-and-filters 'patchelf-and-wrap
              (lambda _
                #| TODO
                cd $out/bin
                patchelf --set-interpreter "$(cat ${ld64})" --set-rpath "${lib.makeLibraryPath buildInputs}:${stdenv.cc.cc.lib}/lib64:${stdenv.cc.libc}/lib64" cnsetuputil2 cnpdfdrv
                patchelf --set-interpreter "$(cat ${ld64})" --set-rpath "${lib.makeLibraryPath buildInputs}:${stdenv.cc.cc.lib}/lib64:${stdenv.cc.libc}/lib64:$out/lib" cnpkbidir cnrsdrvufr2 cnpkmoduleufr2r cnjbigufr2

                wrapProgram $out/bin/cnrsdrvufr2 \
                --prefix LD_LIBRARY_PATH ":" "$out/lib" \
                --set LD_PRELOAD "${libredirect}/lib/libredirect.so" \
                --set NIX_REDIRECTS /usr/bin/cnpkmoduleufr2r=$out/bin/cnpkmoduleufr2r:/usr/bin/cnjbigufr2=$out/bin/cnjbigufr2

                wrapProgram $out/bin/cnsetuputil2 \
                --set LD_PRELOAD "${libredirect}/lib/libredirect.so" \
                --set NIX_REDIRECTS /usr/share/cnsetuputil2=$out/usr/share/cnsetuputil2
                |#
                ))
            (add-after 'patchelf-and-wrap 'install-other-profile-files
              (lambda _
                #| TODO
                cd lib/data/ufr2
                mkdir -p $out/share/caepcm
                install -m 644 *.ICC $out/share/caepcm
                install -m 644 *.icc $out/share/caepcm
                install -m 644 *.PRF $out/share/caepcm
                install -m 644 CnLB* $out/share/caepcm
                |#
                ))
            (add-after 'install-other-profile-files 'install-cnsetuputil
              (lambda _
                #| TODO
                cd cnrdrvcups-utility-${version}/data
                mkdir -p $out/usr/share/cnsetuputil2
                install -m 644 cnsetuputil* $out/usr/share/cnsetuputil2
                |#
                ))
            (add-after 'install-other-profile-files 'wrap-ghostscript
              (lambda _
                #| TODO
                makeWrapper "${ghostscript}/bin/gs" "$out/bin/gs" \
                --prefix LD_LIBRARY_PATH ":" "$out/lib" \
                --prefix PATH ":" "$out/bin"
                |#
                ))
           )))
      (inputs
       (map specification->package
            (list "coreutils"
                  "sed"
                  "ghostscript"
                  "foomatic-filters"
                  "gnu-gettext"
                  "cairo"
                  "pango")))
      (native-inputs
       (map specification->package
            (list "unzip"
                  "automake"
                  "libtool"
                  "autoconf"
                  "pkg-config")))
      (supported-systems '("x86_64-linux"))
      (home-page "https://canon.com")
      (synopsis "Canon UFR II/UFRII LT Printer Driver for Linux")
      (description "This printer driver provides printing functions for UFRII-enabled Canon printers operating under the CUPS (Common UNIX Printing System) environment, a printing system that operates on Linux operating systems. To use this software, please read the online manual before installing the driver.")
      (license (list canon-eula)))))

cndrvcups-lb
