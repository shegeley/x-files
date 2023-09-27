(define-module (x-files file-system drives)

  #:use-module (srfi srfi-1)

  #:use-module (gnu)

  #:use-module (x-files utils records)
  #:use-module (x-files utils files)

  #:use-module ((guix build utils)
                #:select (mkdir-p
                          directory-exists?))

  #:use-module (guix records)

  #:export (drives:file-systems
            drives:mapped-devices
            <drive> drive good-fs? drive:fs
            drive:home make-drive-home-directory
            drive:vender drive:file-systems
            drive:vendor drive:serial))

;; NOTE: would be nice to macros out that `mapped-devices` let
;; ``define-drive''(?)


(define-record-type! drive
  (vendor)
  (version)
  (serial)
  (space (default 'unknown))
  (file-systems)
  (mapped-devices))

;; (define-public samsung-s4en
;;   (let [(mapped-devices
;;          (list (mapped-device
;;                 (source
;;                  (uuid "327a246a-ae1e-4650-bbdb-2660cb966b5d"))
;;                 (target "zonecrypt")
;;                 (type luks-device-mapping))))]
;;     (drive
;;      (vendor "samsung")
;;      (version "4M2QEXF7")
;;      (serial "S4ENNX0N938886")
;;      (mapped-devices mapped-devices)
;;      (file-systems
;;       (list (file-system
;;              (dependencies mapped-devices)
;;              (mount-point "/")
;;              (device "/dev/mapper/zonecrypt")
;;              (type "btrfs"))
;;             (file-system
;;              (dependencies mapped-devices)
;;              (mount-point "/boot/efi")
;;              (device (uuid "9345-926E" 'fat32))
;;              (type "vfat")))))))

(define (drives:file-systems drives)
  (append-map drive:file-systems drives))

(define (drives:mapped-devices drives)
  (append-map drive:mapped-devices drives))

(define (good-fs? fs)
  (let [(mount-point (file-system-mount-point fs))]
    (cond
     ((string=? "/" mount-point) #f)
     ((string-prefix-ci? "/boot" mount-point) #f)
     (else #t))))

(define* (drive:fs
          drive
          #:key (mount-point #f))
  (let* [(file-systems* (filter good-fs? (drive:file-systems drive)))]
    (false-if-exception
     (first (if
             (string? mount-point)
             (filter (lambda (x)
                       (string=? mount-point
                                 (file-system-mount-point x)))
                     file-systems*)
             file-systems*)))))

(define* (drive:home drive user-account
                     #:optional (path "")
                     #:key (mount-point #f))
  "'Pseudo home' directory path for the @code{drive} and @code{user-account}"
  (let* [(mount-point* (file-system-mount-point (apply drive:fs (list drive #:mount-point mount-point))))]
    (string-append
     mount-point* "/" (user-account-home-directory user-account) path)))

(define* (make-drive-home-directory
          drive user-account
          #:key (mount-point #f)
          #:rest args)
  "Creates 'home' directory for the @code{user-account} on @code{drive}, ensures it's permissions and ownership as guix would usually do (NOTE: see @code{(@@ (gnu build activation) activate-user+groups)})"
  (let* [(home (apply drive:home drive user-account args))
         (pwd  (getpwnam (user-account-name user-account)))]
    (with-imported-modules `((guix build utils))
      #~(begin
          (unless (directory-exists? #$home) (mkdir-p #$home))
          (chown #$home (passwd:uid #$pwd) (passwd:gid #$pwd))
          (chmod #$home #o700)))))
