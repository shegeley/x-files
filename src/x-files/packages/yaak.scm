(define-module (x-files packages yaak)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (nonguix multiarch-container)
  #:use-module ((guix licenses) #:prefix license:))

(define-public yaak-appimage
 (let* [(version "2025.4.0")
        (filename (string-append "yaak_" version "_amd64.AppImage"))
        (tgz (string-append filename ".tar.gz"))
        (uri (string-append
              "https://github.com/mountain-loop/"
              "yaak/releases/download/v"
              version "/" tgz))
        (hash "0swq3sjnim5np6xp1va59sj7h9yl4glj94dhbmn6x4dxc8ii27sv")]
  (package
   (name "yaak")
   (version version)
   (source (origin
            (method url-fetch)
            (uri uri)
            (sha256 (base32 hash))))
   (build-system copy-build-system)
   (arguments
    (list
     #:validate-runpath? #f
     #:install-plan `'((,filename "/shared/bin/yaak"))))
   (supported-systems (list "x86_64-linux"))
   (home-page "https://yaak.app")
   (synopsis "API client for modern developers")
   (description "Fast, offline, and Git friendly app for HTTP, GraphQL, WebSockets, SSE, and gRPC")
   (license license:expat))))

(define-public yaak
  (nonguix-container->package
   (nonguix-container
    (name "yaak")
    (wrap-package yaak-appimage)
    (run "/shared/bin/yaak")
    (packages '())
    (link-files '("share"))
    (description ""))))

yaak
