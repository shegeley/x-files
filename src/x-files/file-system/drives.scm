(define-module (x-files file-system drives)

  #:use-module (srfi srfi-1)

  #:use-module (gnu)

  #:use-module (x-files utils records)

  #:use-module (guix records)

  #:export (drives:file-systems
            drives:mapped-devices
            <drive>
            drive))

;; NOTE: would be nice to macros out that `mapped-devices` let
;; ``define-drive''(?)

(define-record-type! drive
  (vendor)
  (version)
  (serial)
  (space (default 'unknown))
  (file-systems)
  (mapped-devices))

(define (drives:file-systems drives)
  (append-map
   (lambda (d)
     (match-record d <drive>
                   (file-systems)
                   file-systems)) drives))

(define (drives:mapped-devices drives)
  (append-map
   (lambda (d)
     (match-record d <drive>
                   (mapped-devices)
                   mapped-devices)) drives))

(define (good-fs? fs)
  (make-parameter
   (let [(mount-point (file-system-mount-point fs))]
     (cond
      ((string=? "/" mount-point) #f)
      ((string-prefix-ci? mount-point "/boot") #f)
      (else #t)))))

(define* (drive:fs drive
                   #:key (mount-point #f))
  (match-record
   drive (@@ (x-files file-system drives) <drive>)
   (file-systems)
   (let* [(file-systems* (filter (lambda (x) ((good-fs? x))) file-systems))
          (file-system (if
                        (string? mount-point)
                        (first
                         (filter (lambda (x)
                                   (string=? mount-point
                                             (file-system-mount-point x)))
                                 file-systems*))
                        (first file-systems*)))]
     file-system)))

;; Example:
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
