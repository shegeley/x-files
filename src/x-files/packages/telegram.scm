(define-module (x-files packages telegram)
 #:use-module (gnu packages base)
 #:use-module (gnu packages bootstrap)
 #:use-module (gnu packages compression)
 #:use-module (gnu packages elf)
 #:use-module (gnu packages fontutils)
 #:use-module (gnu packages gcc)
 #:use-module (gnu packages gl)
 #:use-module (gnu packages gtk)
 #:use-module (gnu packages glib)
 #:use-module (gnu packages linux)
 #:use-module (gnu packages ncurses)
 #:use-module (gnu packages nss)
 #:use-module (gnu packages pulseaudio)
 #:use-module (gnu packages xorg)
 #:use-module (nonguix build-system binary)
 #:use-module ((nonguix licenses) #:prefix license:)
 #:use-module (gnu packages freedesktop)

 #:use-module (guix download)
 #:use-module (guix gexp)
 #:use-module (guix packages)
 #:use-module (guix store)
 #:use-module (guix utils))

;; <shegeley@prime:~/Downloads>
;; zsh 5018 % tree Telegram
;; Telegram
;; ├── Telegram
;; └── Updater

;; <shegeley@prime:~/Downloads>
;; zsh 5014 [127] % readelf -d Telegram

;; Dynamic section at offset 0xae9d610 contains 37 entries:
;;   Tag        Type                         Name/Value
;;  0x0000000000000001 (NEEDED)             Shared library: [libfontconfig.so.1]
;;  0x0000000000000001 (NEEDED)             Shared library: [libfreetype.so.6]
;;  0x0000000000000001 (NEEDED)             Shared library: [libX11.so.6]
;;  0x0000000000000001 (NEEDED)             Shared library: [libgio-2.0.so.0]
;;  0x0000000000000001 (NEEDED)             Shared library: [libgobject-2.0.so.0]
;;  0x0000000000000001 (NEEDED)             Shared library: [libglib-2.0.so.0]
;;  0x0000000000000001 (NEEDED)             Shared library: [libdl.so.2]
;;  0x0000000000000001 (NEEDED)             Shared library: [libm.so.6]
;;  0x0000000000000001 (NEEDED)             Shared library: [libpthread.so.0]
;;  0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
;;  0x0000000000000001 (NEEDED)             Shared library: [ld-linux-x86-64.so.2]
;;  0x000000000000000c (INIT)               0xea000

(define* (telegram-desktop*
          #:key
          (version "5.14.2")
          (checksum "05ljcb1llyci7ir3xx6zpyllgg5yzcnp6y7w5f91ksmwx7a0rcdc"))
 (package
  (name "telegram-desktop")
  (version version)
  (source (origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/"
                 "telegramdesktop/tdesktop/releases/download/"
                 "v" version "/tsetup." version ".tar.xz"))
           (sha256 (base32 checksum))))
  (build-system binary-build-system) ;; <- TODO: change to binary-build-system
  (arguments
   (list
    #:install-plan #~'(("Telegram" "bin/Telegram"))
    #:patchelf-plan #~'(("Telegram" ("gcc" "glib" "freetype" "libx11" "glibc" "fontconfig-minimal" "wayland" "gtk")))
    #:validate-runpath? #f
    #:phases
    #~(modify-phases %standard-phases
       (add-after 'unpack 'fix-binary
        (lambda* (#:key outputs inputs #:allow-other-keys)
         (let [(out (assoc-ref outputs "out"))
               (ld.so (search-input-file inputs #$(glibc-dynamic-linker)))
               (patchelf (search-input-file inputs "/bin/patchelf"))]
          (system* patchelf "--set-interpreter" ld.so out "/bin/Telegram") #t))))))
  (supported-systems '("x86_64-linux"))
  (synopsis "Telegram desktop precompiled binary")
  (description "Package provides latest versions of Telegram desktop by patching precompiled official binaries")
  (home-page "https://desktop.telegram.org/")
  (native-inputs (list patchelf))
  (inputs (list
           (list gcc "lib")
           libx11
           libxrandr
           glibc
           glib
           gtk
           freetype
           fontconfig
           wayland
           pulseaudio))
  (license (license:nonfree "https://github.com/telegramdesktop/tdesktop/blob/dev/LICENSE"))))

(telegram-desktop*)
