(define-module (x-files packages nm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages base)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public network-manager-l2tp
  (package
    (name "network-manager-l2tp")
    (version "1.20.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nm-l2tp/NetworkManager-l2tp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0znv57fkg2l7pjbgdn9g3issvf5mb252d8hfm3pxf6wl21cc6rh2"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "--enable-absolute-paths"
         "--localstatedir=/var"
         "--with-gtk4=yes")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-path
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (let* ((xl2tpd (search-input-file inputs "/sbin/xl2tpd"))
                     (ipsec
                      (search-input-file inputs "/sbin/ipsec"))
                     (modprobe
                      (search-input-file inputs "/bin/modprobe")))
                (for-each
                 (lambda (file)
                   (substitute* file
                     (("/usr/bin/xl2tpd") xl2tpd)
                     (("/usr/bin/ipsec") ipsec)
                     (("/sbin/modprobe") modprobe)))
                 '("shared/utils.c"
                   "src/nm-l2tp-service.c"))))))))
    (native-inputs
     (list gettext-minimal
           intltool
           autoconf
           automake
           libtool
           (list glib "bin")
           pkg-config))
    (inputs
     (list gtk+
           gtk
           (list gtk "bin")
           kmod
           ppp
           openssl
           nss
           libnma
           libsecret
           network-manager
           strongswan
           xl2tpd))
    (home-page "https://github.com/nm-l2tp/NetworkManager-l2tp")
    (synopsis "l2tp plug-in for NetworkManager")
    (description
     "NetworkManager-l2tp is a VPN plugin for NetworkManager 1.20 and later which provides support for L2TP and L2TP/IPsec (i.e. L2TP over IPsec) connections.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-l2tp")))))
