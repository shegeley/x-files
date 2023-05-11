(define-module (x-files packages databases)
  #:use-module (gnu)

  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages gcc)
  #:use-module (gnu packages compression)

  #:use-module (nonguix build-system binary))

(define sspl
  ((@@ (guix licenses) license)
   "SSPL"
   "https://raw.githubusercontent.com/mongodb-js/compass/main/LICENSE"
   "Bullshit mongo license"))

(define-public compass
  (let* ((inputs* (list
                   "pth"
                   "glib"
                   "dbus-glib"
                   "gio"
                   "nss"
                   "cups"
                   "dbus"
                   "nss-utils"
                   "atk"
                   "cairo"
                   "nspr@4"
                   "openlibm"
                   "libdrm"
                   "libtool"
                   "libxcb"
                   "libxext"
                   "libfixes"
                   "libxdamage"
                   "libxkbcommon"
                   "libxrandr"
                   "libx11")))
    (package
      (name "mongo-compass")
      (version "1.36.4")
      (source (origin
                (method url-fetch)
                (uri
                 ;;https://github.com/mongodb-js/compass/releases/download/v1.36.4/mongodb-compass-1.36.4-linux-x64.tar.gz
                 (string-append
                  "https://github.com/mongodb-js/compass/releases/download/v" version "/ /mongodb-compass-" version "-linux-x64.tar.gz"))
                (sha256
                 (base32 "1rdfg3fri8z47p8cyky98y47nw993h039a5phb2f90vscbxlyigc"))))
      (build-system binary-build-system)
      (supported-systems '("x86_64-linux"))
      ;; /MongoDB Compass-linux-x64$ readelf -d MongoDB\ Compass
      ;; Dynamic section at offset 0x99df518 contains 61 entries:
      ;;   Tag        Type                         Name/Value
      ;;  0x000000000000000f (RPATH)              Library rpath: [$ORIGIN]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libffmpeg.so]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libdl.so.2]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libpthread.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libgobject-2.0.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libglib-2.0.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libgio-2.0.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libnss3.so]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libnssutil3.so]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libsmime3.so]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libnspr4.so]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libatk-1.0.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libatk-bridge-2.0.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libcups.so.2]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libdbus-1.so.3]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libdrm.so.2]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libgtk-3.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libpango-1.0.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libcairo.so.2]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libm.so.6]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libX11.so.6]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libXcomposite.so.1]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libXdamage.so.1]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libXext.so.6]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libXfixes.so.3]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libXrandr.so.2]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libgbm.so.1]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libexpat.so.1]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libxcb.so.1]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libxkbcommon.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libasound.so.2]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libatspi.so.0]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libgcc_s.so.1]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
      ;;  0x0000000000000001 (NEEDED)             Shared library: [ld-linux-x86-64.so.2]

      ;; (arguments
      ;;  (list
      ;;   #:patchelf-plan #~(()))
      ;;  #:install-plan #~(()))
      ;; (inputs
      ;;  `())
      (license sspl)
      (synopsis
       "Easily explore and manipulate your database with Compass, the GUI for MongoDB")
      (description
       "Intuitive and flexible, Compass provides detailed schema visualizations, real-time performance metrics, sophisticated querying abilities, and much more.")
      (home-page "https://www.mongodb.com/try/download/compass"))))
