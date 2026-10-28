(define-module (x-files packages pluely)
  #:use-module ((guix licenses)               #:prefix license:)
  #:use-module ((guix packages)               #:select (package
                                                        origin
                                                        base32))
  #:use-module ((gnu packages)                #:select (specification->package))
  #:use-module ((guix download)               #:select (url-fetch))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:use-module (guix gexp))

;; Pluely is a Tauri (Rust + WebKitGTK) app, packaged from the upstream
;; prebuilt .deb exactly like (x-files packages yaak): binary-build-system
;; unpacks the .deb itself, the binary only needs patchelf'd RPATHs.
(define %pluely-libs
  '("webkitgtk-for-gtk3" "gtk+" "cairo" "gdk-pixbuf" "glib" "libsoup"
    "openssl" "pulseaudio" "libxcb" "gcc-toolchain"))

(define-public pluely
  (package
    (name "pluely")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/iamsrikanthnani/pluely/releases/download/app-v"
                           version "/Pluely_" version "_amd64.deb"))
       (sha256
        (base32 "0anhcaqdq2hcks1m90c80prgh8qd54a4lymwkb8p205hnp5f13w3"))))
    (build-system binary-build-system)
    (arguments
     (list
      #:install-plan #~'(("usr/bin"   "bin")
                         ("usr/lib"   "lib")
                         ("usr/share" "share"))
      #:patchelf-plan
      `'(("usr/bin/pluely" ,%pluely-libs))))
    (inputs (map specification->package %pluely-libs))
    (supported-systems '("x86_64-linux"))
    (home-page "https://pluely.com")
    (synopsis "Privacy-first AI assistant overlay for meetings and interviews")
    (description
     "Pluely is an open source alternative to Cluely: a lightweight desktop
overlay that listens to system audio and the microphone and answers questions
with the AI provider of your choice during meetings, interviews and
conversations.  This package installs the upstream prebuilt Debian package.")
    (license license:gpl3)))
