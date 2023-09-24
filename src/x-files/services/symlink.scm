(define-module (x-files services symlink)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (srfi srfi-1)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)
  #:use-module (x-files file-system drives)

  #:use-module (gnu services configuration)

  #:use-module (gnu home services)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix build utils)
  #:use-module (guix build syscalls)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:export (safe-symlinker-configuration))

(define (modules-selector name)
  (or ((@@ (guix modules) guix-module-name?) name)
      (match name
        (('x-files 'utils ..) #t)
        (else #f))))

(define-configuration/no-serialization safe-symlinker-configuration
  (src string "source")
  (dst string "destination"))

(define (activation config)
  (match-record
   config <safe-symlinker-configuration>
   (src dst)
   (with-imported-modules
    (source-module-closure
     '((x-files utils files))
     #:select? modules-selector)
    #~(safe-move&symlink #$src #$dst))))

(define-public home-safe-symlinker-service-type
  ;; (shepherd-service
  ;;  (provision )
  ;;  (requirement '(udev))
  ;;  (documentation "Map a device node using Linux's device mapper.")
  ;;  (start #~(lambda () #$(open source targets)))
  ;;  (stop #~(lambda _ (not #$(close source targets))))
  ;;  (respawn? #f))
  (service-type
   (name 'home-symlink)
   (extensions
    (list
     (service-extension
      home-activation-service-type
      activation)))
   (description "Move and symlink file")))
