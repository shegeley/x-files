(define-module (x-files packages amneziawg)
  ;; Packages originally authored by Stephan (@a_mongoose3 on Telegram)
  #:use-module ((guix packages)         #:select (package
                                                  base32
                                                  origin))
  #:use-module ((guix git-download)     #:select (git-fetch
                                                  git-reference
                                                  git-file-name))
  #:use-module ((guix gexp)             #:select (gexp local-file file-append))
  #:use-module ((guix utils)                #:select (cc-for-target))
  #:use-module ((guix build-system go)      #:select (go-build-system))
  #:use-module ((guix build-system gnu)     #:select (gnu-build-system))
  #:use-module ((guix build-system linux-module) #:select (linux-module-build-system))
  #:use-module ((guix licenses)             #:prefix license:)
  #:use-module ((gnu packages vpn)          #:select (wireguard-tools))
  #:use-module ((gnu packages dns)          #:select (openresolv))
  #:use-module ((gnu packages base)         #:select (coreutils))
  #:use-module ((gnu packages bash)         #:select (bash))
  #:use-module ((gnu packages linux)        #:select (procps iproute iptables))
  #:use-module ((gnu packages golang-build) #:select (go-golang-org-x-net
                                                       go-golang-org-x-sys
                                                       go-golang-org-x-crypto))
  #:use-module ((gnu packages golang-web)   #:select (go-github-com-things-go-go-socks5))
  #:use-module ((gnu packages golang-xyz)   #:select (go-github-com-makenowjust-heredoc-v2
                                                       go-github-com-akamensky-argparse
                                                       go-github-com-go-ini-ini
                                                       go-github-com-landlock-lsm-go-landlock
                                                       go-gvisor-dev-gvisor
                                                       go-suah-dev-protect)))

(define-public go-github-com-amnezia-vpn-amneziawg-go
  (package
    (name "go-github-com-amnezia-vpn-amneziawg-go")
    (version "0.2.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/amnezia-vpn/amneziawg-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16l6qwq5rzqpqkvziiaz8g0q59b6pdq50iv7cs9bfyk0ylq9cs94"))))
    (native-inputs
     (list go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-crypto))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/amnezia-vpn/amneziawg-go"))
    (home-page "https://github.com/amnezia-vpn/amneziawg-go")
    (synopsis "Go Implementation of AmneziaWG")
    (description
     "AmneziaWG is a contemporary version of the WireGuard protocol.  It's a
fork of WireGuard-Go and offers protection against detection by Deep Packet
Inspection (DPI) systems.  At the same time, it retains the simplified
architecture and high performance of the original.")
    (license license:expat)))

(define-public amneziawg-tools
  (package
   (inherit wireguard-tools)
   (name "amneziawg-tools")
   (version "1.0.20260223")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/amnezia-vpn/amneziawg-tools")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05jyfqdbk9km0dahmddif7dghp1zslz458yd2hf18fha456qrii6"))))
   (inputs (list
            openresolv
            coreutils
            go-github-com-amnezia-vpn-amneziawg-go
            bash
            procps
            iproute
            iptables))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags
      (list ,(string-append "CC=" (cc-for-target))
            "--directory=src"
            "WITH_BASHCOMPLETION=yes"
            ;; Install the 'simple and dirty' helper script wg-quick(8).
            "WITH_WGQUICK=yes"
            (string-append "PREFIX=" (assoc-ref %outputs "out"))
            ;; Currently used only to create an empty /etc/wireguard directory.
            (string-append "SYSCONFDIR=no-thanks"))
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (delete 'configure)
                     (add-after 'install 'install-contrib-docs
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((out (assoc-ref outputs "out"))
                                         (doc (string-append out "/share/doc/amneziawg-tools")))
                                    (copy-recursively "contrib/" doc))))
                     (add-after 'install 'wrap-awg-quick
                                (lambda* (#:key inputs outputs #:allow-other-keys)
                                  (wrap-program (string-append (assoc-ref outputs "out")
                                                               "/bin/awg-quick")
                                                `("PATH" ":" prefix
                                                  (,(dirname (search-input-file inputs "/sbin/ip"))
                                                   ,(dirname (search-input-file inputs "/sbin/iptables"))
                                                   ,(dirname (search-input-file inputs "/sbin/sysctl"))
                                                   ,(dirname (search-input-file inputs "/sbin/resolvconf"))
                                                   ,(dirname (search-input-file inputs "/bin/amneziawg-go"))
                                                   ,(dirname (search-input-file inputs "/bin/cp"))))))))))))

(define-public wireproxy-awg
  (package
    (name "wireproxy-awg")
    (version "1.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/artem-russkikh/wireproxy-awg")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p4spqgl1yg0zqi2lmn2gi6z2szlvm955h37dmfwf9ajd82jwcm3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/artem-russkikh/wireproxy-awg/cmd/wireproxy"
      #:unpack-path "github.com/artem-russkikh/wireproxy-awg"
      #:test-subdirs #~(list "../../...")))
    (native-inputs
     (list go-github-com-makenowjust-heredoc-v2
           go-github-com-amnezia-vpn-amneziawg-go
           go-github-com-akamensky-argparse
           go-github-com-go-ini-ini
           go-github-com-landlock-lsm-go-landlock
           go-gvisor-dev-gvisor
           go-github-com-things-go-go-socks5
           go-golang-org-x-net
           go-suah-dev-protect))
    (home-page "https://github.com/artem-russkikh/wireproxy-awg")
    (synopsis "Amneziawg client that exposes itself as a socks5 proxy")
    (description
     "wireproxy-awg is a completely userspace application that connects to a
wireguard peer, and exposes a socks5/http proxy or tunnels on the machine.")
    (license license:isc)))

(define-public amneziawg-kernel-module
  (package
   (name "amneziawg-kernel-module")
   (version "6006d4467471ed5893cfb2bd27ca054ac8351705")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/amnezia-vpn/amneziawg-linux-kernel-module")
                  (commit "26f5df04ec47b85c7aa09523cb89cf4c5e6c9508")))
            (sha256
             (base32
              "0dana6z6z32f7f7mcf52b90vb2m90rsjla9fz7x04pcqakz2926h"))))
   (build-system linux-module-build-system)
   (arguments
    `(#:tests? #f
      #:modules ((guix build linux-module-build-system)
                 (guix build utils)
                 (ice-9 popen)
                 (ice-9 textual-ports))
      #:phases
      (modify-phases %standard-phases
                     (add-before 'build 'change-directory
                                 (λ _
                                   (chdir "./src")
                                   #t))
                     (add-after 'build 'build-patch
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (let* ((patch-builder "../kernel-tree-scripts/create-patch.sh")
                                         (port (open-input-pipe patch-builder))
                                         (str (get-string-all port)))
                                    (close-pipe port)
                                    (call-with-output-file "wireguard.patch"
                                      (lambda (port)
                                        (format port "~a" str))))
                                  #t))
                     (add-after 'install 'install-patch
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (install-file "wireguard.patch"
                                                (assoc-ref %outputs "kernel-patch"))
                                  #t))
                     (add-before 'install-license-files 'reset-cwd
                                 (lambda _
                                   (chdir "..")
                                   #t)))))
   (outputs '("out" "kernel-patch"))
   (home-page "https://github.com/amnezia-vpn/amneziawg-linux-kernel-module")
   (synopsis "AmneziaWG Linux kernel module")
   (description "Linux kernel module for Amnezia Wireguard support.

Official documentation on module installation is currently outdated,
see linux-loadable-module-service-type.")
   (license license:gpl2)))
