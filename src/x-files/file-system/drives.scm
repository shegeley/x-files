(define-module (x-files file-system drives)

  #:use-module (srfi srfi-1)

  #:use-module (gnu)

  #:use-module (x-files utils records)
  #:use-module (x-files utils files)

  #:use-module (guix records)

  #:export (drives:file-systems
            drives:mapped-devices
            <drive> drive good-fs? drive:fs
            drive:movelink drive:home
            make-drive-home-directory
            drive:movelink))

;; NOTE: would be nice to macros out that `mapped-devices` let
;; ``define-drive''(?)

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

(define-record-type! drive
  (vendor)
  (version)
  (serial)
  (space (default 'unknown))
  (file-systems)
  (mapped-devices))

(define (drives:file-systems drives)
  (append-map drive:file-systems drives))

(define (drives:mapped-devices drives)
  (append-map drive:mapped-devices drives))

(define (good-fs? fs)
  (let [(mount-point (file-system-mount-point fs))]
    (cond
     ((string=? "/" mount-point) #f)
     ((string-prefix-ci? mount-point "/boot") #f)
     (else #t))))

(define* (drive:fs drive
                   #:key (mount-point #f))
  (match-record
   drive (@@ (x-files file-system drives) <drive>)
   (file-systems)
   (let* [(file-systems* (filter (lambda (x) (good-fs? x)) file-systems))
          (file-system (first
                        (if
                         (string? mount-point)
                         (filter (lambda (x)
                                   (string=? mount-point
                                             (file-system-mount-point x)))
                                 file-systems*)
                         file-systems*)))]
     file-system)))

(define* (drive:home drive user-account
                     #:optional (path "")
                     #:key (mount-point #f))
  "'Pseudo home' directory path for the @code{drive} and @code{user-account}"
  (let* [(fs (drive:fs drive #:mount-pount mount-point))]
    (string-append fs "/" (user-account-home-directory user-account) path)))

(define* (make-drive-home-directory
          drive user-account
          #:key (mount-point #f))
  "Creates 'home' directory for the @code{user-account} on @code{drive}, ensures it's permissions and ownership as guix would usually do (NOTE: see @code{(@@ (gnu build activation) activate-user+groups)})"
  (let* [(home (drive:home drive user-account #:mount-pount mount-point))
         (pwd  (getpwnam (user-account-name user-account)))]
    (mkdir-p home)
    (chown home (passwd:uid pwd) (passwd:gid pwd))
    (chmod home #o700)))

(define* (drive:movelink drive src rel-dst
                         #:key (mount-point #f))
  "Moves and symlinks 'src' to the drive's relative destination ('rel-dst'). Does all checks for safety"
  (let* [(fs (drive:fs drive #:mount-point mount-point))
         (mount-point (file-system-mount-point fs))]
    (safe-move&symlink
     src (string-append mount-point "/" rel-dst))))
