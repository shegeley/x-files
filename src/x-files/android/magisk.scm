(define-module (x-files android magisk)
 #:use-module (guix packages)
 #:use-module (guix download)
 #:use-module (gnu packages compression)
 #:use-module (guix build-system copy)
 #:use-module ((guix licenses) #:prefix license:))

(define-public magisk
 (package
  (name "magisk")
  (version "28.0")
  (source
   (origin
    (method url-fetch)
    (uri (string-append "https://github.com/topjohnwu/Magisk/releases/download/v28.0/Magisk-v" version ".apk"))
    (sha256 (base32 "1qkzhzzb00n9jpws0m9xpj9iwd5kxz7bs2zjwm0imazq56nfn7hv"))))
  (build-system copy-build-system)
  (synopsis "The Magic Mask for Android")
  (home-page "https://github.com/topjohnwu/Magisk")
  (description "Magisk is a suite of open source software for customizing Android, supporting devices higher than Android 6.0.
Some highlight features:

@itemize
@item MagiskSU: Provide root access for applications
@item Magisk Modules: Modify read-only partitions by installing modules
@item MagiskBoot: The most complete tool for unpacking and repacking Android boot images
@item Zygisk: Run code in every Android applications' processes
@end itemize")
  (license license:gpl3)))
