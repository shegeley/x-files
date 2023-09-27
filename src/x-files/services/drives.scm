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

  #:export (movelinker-configuration
            movelinker-service-type
            drive-movelinker-services))

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
         (match-record config <movelinker-configuration>
                       (src dst)
           (with-imported-modules
               (source-module-closure
                '((x-files utils files))
                #:select? modules-selector)
             #~(safe-move&symlink #$src #$dst))))
     #$configs))

(define-public movelinker-service-type
  (service-type
   (name 'movelinker)
   (compose concatenate)
   (extend append)
   (default-value '())
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
          drive dirs user-account
          #:key
          (mount-point #f))
  "Moves @code{dirs} (dir can be file) relative to @code{user-account} home to non-root @code{drive} relative to to @code{drive:home} and symlinks them with original directory. Ensures that drive's directory has correct ownership&permisions for the @code{user-account}"
  (let* [(drive:name (string->symbol (string-append
                                      (drive:vendor drive) "-"
                                      (drive:serial drive))))
         (src (lambda (src)
                (string-append
                 (user-account-home-directory user-account)
                 "/" src)))
         (srcs (map src dirs))
         (_ (display srcs))
         (dst (lambda (src)
                (drive:home drive user-account
                            (string-append "/" src))))
         (dsts (map dst dirs))
         (_ (display dsts))
         (confs (map
                 (lambda (dst src)
                   (movelinker-configuration
                    (dst dst)
                    (src src)))
                 dsts srcs))
         (make-home-dir-gexp
          (with-imported-modules
              (source-module-closure
               '((x-files file-systems drives)
                 (gnu system accounts))
               #:select? modules-selector)
            (make-drive-home-directory
             drive user-account
             #:mount-point mount-point)))]
    (list
     (simple-service
      (symbol-append 'ensure- drive:name '-home)
      activation-service-type make-home-dir-gexp)
     (service movelinker-service-type confs))))
