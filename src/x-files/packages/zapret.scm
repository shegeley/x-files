(define-module (x-files packages zapret)
  ;; Packages originally authored by Stephan (@a_mongoose3 on Telegram)
  #:use-module ((guix packages)         #:select (package
                                                  base32
                                                  origin))
  #:use-module ((guix git-download)     #:select (git-fetch
                                                  git-reference
                                                  git-file-name))
  #:use-module ((guix build-system gnu) #:select (gnu-build-system))
  #:use-module ((guix licenses)         #:prefix license:)
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages linux)    #:select (libcap libmnl libnetfilter-queue libnfnetlink nftables))
  #:use-module ((gnu packages compression) #:select (zlib))
  #:use-module ((gnu packages lua)      #:select (luajit))
  #:use-module ((gnu packages curl)     #:select (curl))
  #:use-module ((gnu packages dns)      #:select (isc-bind)))

(define-public zapret2
  (package
   (name "zapret2")
   (version "0.9.5")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/bol-van/zapret2")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1awl8ngh6x6sdn04az1rrsw1v7ignaz8bp4d3067li8gc9bp6cnd"))))
   (build-system gnu-build-system)
   (native-inputs (list
                   pkg-config
                   libcap
                   zlib
                   libmnl
                   libnetfilter-queue
                   libnfnetlink))
   (inputs (list
            luajit
            nftables
            curl
            `(,isc-bind "utils")))
   (arguments
    `(#:make-flags (list "CC=gcc")
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-output-path
          (lambda _
            (substitute* "Makefile"
              (("TGT := binaries/my")
               (string-append "TGT := " (assoc-ref %outputs "out") "/libexec/zapret2/nfq2")))))
        (add-after 'unpack 'fix-blockcheck-file
          (lambda _
            (substitute* "blockcheck2.sh"
              (("^ZAPRET_BASE=.*")
               (string-append "ZAPRET_BASE=" (assoc-ref %outputs "out") "/libexec/zapret2\n"))
              (("^ZAPRET_CONFIG=.*")
               "ZAPRET_CONFIG=/etc/zapret2/config\n")
              (("^ZAPRET_CONFIG_DEFAULT=.*")
               (string-append "ZAPRET_CONFIG_DEFAULT=" (assoc-ref %outputs "out") "/etc/zapret2/config.default\n"))
              (("^NFQWS2=.*")
               (string-append "NFQWS2=" (assoc-ref %outputs "out") "/libexec/zapret2/nfq2/nfqws2\n"))
              (("^MDIG=.*")
               (string-append "MDIG=" (assoc-ref %outputs "out") "/libexec/zapret2/nfq2/mdig\n")))))
        (add-after 'unpack 'patch-sysv-init-files
          (lambda _
            (substitute* "init.d/sysv/zapret2"
              (("^EXEDIR=.*")
               (string-append "EXEDIR=" (assoc-ref %outputs "out") "/libexec/zapret2\n"))
              (("^ZAPRET_BASE=.*")
               (string-append "ZAPRET_BASE=" (assoc-ref %outputs "out") "/libexec/zapret2\n"
                              "ZAPRET_CONFIG=/etc/zapret2/config\n"
                              "ZAPRET_CONFIG_DEFAULT=" (assoc-ref %outputs "out") "/etc/zapret2/config.default\n"
                              "[ -f $ZAPRET_CONFIG ] || cp $ZAPRET_CONFIG_DEFAULT $ZAPRET_CONFIG\n"))
              (("Usage\\: \\$SCRIPT")
               "Usage: zapret2"))))
        (add-after 'build 'copy-files
          (lambda _
            (copy-recursively
             "lua"
             (string-append (assoc-ref %outputs "out") "/libexec/zapret2/lua/"))
            (copy-recursively
             "common"
             (string-append (assoc-ref %outputs "out") "/libexec/zapret2/common"))
            (copy-recursively
             "ipset"
             (string-append (assoc-ref %outputs "out") "/libexec/zapret2/ipset"))
            (mkdir (string-append (assoc-ref %outputs "out") "/etc"))
            (mkdir (string-append (assoc-ref %outputs "out") "/etc/zapret2"))
            (copy-file
             "config.default"
             (string-append (assoc-ref %outputs "out") "/etc/zapret2/config.default"))
            (copy-file
             "init.d/sysv/functions"
             (string-append (assoc-ref %outputs "out") "/libexec/zapret2/functions"))
            (mkdir (string-append (assoc-ref %outputs "out") "/bin"))
            (copy-file
             "init.d/sysv/zapret2"
             (string-append (assoc-ref %outputs "out") "/bin/zapret2"))
            (copy-recursively
             "blockcheck2.d"
             (string-append (assoc-ref %outputs "out") "/libexec/zapret2/blockcheck2.d"))
            (copy-file
             "blockcheck2.sh"
             (string-append (assoc-ref %outputs "out") "/bin/zapret2-blockcheck"))))
        (add-after 'copy-files 'wrap-scripts
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (wrap-program (string-append (assoc-ref outputs "out")
                                                  "/bin/zapret2")
                                   `("PATH" ":" prefix
                                     (,(dirname (search-input-file inputs "/sbin/nft")))))
                     (wrap-program (string-append (assoc-ref outputs "out")
                                                  "/bin/zapret2-blockcheck")
                                   `("PATH" ":" prefix
                                     (,(dirname (search-input-file inputs "/bin/curl"))
                                      ,(dirname (search-input-file inputs "/bin/nslookup")))))))
        (delete 'configure)
        (delete 'install))))
   (home-page "https://github.com/bol-van/zapret2")
   (synopsis "zapret2 is a dpi bypass platform configured in lua")
   (description "zapret2 is a packet manipulator primarily designed to perform
various autonomous real-time attacks on Deep Packet Inspection (DPI) systems.
Its main objective is to bypass resource blocks or protocol restrictions.
However, zapret2's capabilities are not limited to this; its architecture
allows for other types of packet manipulation, such as bidirectional
(client-server) protocol obfuscation to hide traffic from DPI, among other
applications.

Configuration file is set to /etc/zapret2/config
renamed blockcheck2.sh -> zapret2-blockcheck and moved it to bin")
   (license license:expat)))

zapret2
