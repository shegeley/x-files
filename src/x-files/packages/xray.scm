(define-module (x-files packages xray)
  #:use-module (guix build-system copy)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module ((gnu packages compression) #:select (unzip))

  #:use-module ((guix licenses) #:prefix license:))

(define xray-core-version "26.2.6")

(define target->xray-core-target
  ;; TODO: add others
  '(("x86_64-linux"  . "linux-64")))

(define targets (map car target->xray-core-target))

(define target->xray-core-server-hash
  ;; TODO: add other
  '(("x86_64-linux"  . "11g3z34sncmpid8fry7g0gh0nhfc5n2d9hm1zw3a81z2ardm7ki9")))

(define-public xray-core
  (let* [(target (or (%current-target-system) (%current-system)))
         (xray-core-target (assoc-ref target->xray-core-target target))
         (zip-filename (string-append "Xray-" xray-core-target ".zip"))
         (hash (assoc-ref target->xray-core-server-hash target))]
    (package
      (name "xray-core")
      (version (string-append "v" xray-core-version))
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/"
                      "XTLS/Xray-core/"
                      "releases/download/"
                      "v26.2.6/Xray-linux-64.zip"))
                (sha256 (base32 hash))))
      (build-system copy-build-system)
      (inputs (list unzip))
      (arguments
       (list
        #:install-plan #~`(("xray" "/bin/xray"))
        #:phases #~(modify-phases %standard-phases
                     (replace 'unpack
                       (lambda _
                         (invoke "unzip" #$source)
                         (chmod "xray" #o755))))))
      (supported-systems targets)
      (home-page "")
      (synopsis "Project X originates from XTLS protocol, providing a set of network tools such as Xray-core and REALITY")
      (description "Xray-core is a superset of v2ray-core, with better overall performance and enhancements such as XTLS, and it'scompletely compatible with v2ray-core functionality and configuration.

@itemize
@item Only one executable file, including ctl functionality, run is the default command
@item Configuration iscompletelycompatible, environment variables and API calls need to be changed to start with XRAY_
@item Exposed raw protocol's ReadV on all platforms
@item Provides complete VLESS & Trojan XTLS support, both with ReadV
@item Provides multiple XTLS flow control modes, unrivaled performance!
@end itemize")
      (license license:mpl2.0))))
