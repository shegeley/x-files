(define-module (x-files services drives)
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

(define-configuration/no-serialization movelinker-configuration
  #| Safely moves + symlinks files |#
  (src string "Source")
  (dst string "Destination"))

(define (activation configs)
  #~(map
     #$(lambda (config)
         (match-record
          config <movelinker-configuration>
          (src dst)
          (with-imported-modules
           (source-module-closure
            '((x-files utils files))
            #:select? modules-selector)
           #~(safe-move&symlink #$src #$dst))))
     #$configs))

(define-public movelinker-service-type
  (service-type
   (name 'symlinker)
   (compose concatenate)
   (extend append)
   (extensions
    (list
     (service-extension home-activation-service-type activation)))
   (description "Safely move and symlink files")))

(define (modules-selector name)
  (or ((@@ (guix modules) guix-module-name?) name)
      (match name
        (('x-files 'utils ..) #t)
        (else #f))))

(define* (drive-movelinker-services
          drive dirs
          user-account
          #:key
          (mount-point #f))
  ;; NOTE: THAT'S WHAT'S WANTED

  "Moves @code{dirs} relative to @code{user-account} home to non-root drive @code{drive} according to following scheme «<drive's primary file-system mount point>/<user-home>/dir», symlinks them with original directory afterwards and ensures that drive's directory has correct ownership&permisions for the @code{user-account}"
  (let* [(fs (drive:fs drive))
         (dst (lambda (src)
                (drive:home drive user-accoung (string-append "/" src))))
         (dirs* (map dst dirs))
         (confs (map
                 (lambda (d*)
                   (movelinker-configuration
                    (dst d*)
                    (src d*))) dirs*))]
    (list
     (simple-service
      'ensure-permissions activation-service-type
      (with-imported-modules
       (source-module-closure
        '((x-files file-systems drives))
        #:select? modules-selector)
       #~(make-drive-home-directory
          #$drive #$user-account
          #:mount-point #$mount-point)))
     (simple-service
      'drive-movelinker-service-type movelinker-service-type
      confs))))
