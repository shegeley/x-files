(define-module (x-files features drives)
  #:use-module ((rde features system) #:select (feature-file-systems))
  #:use-module ((x-files file-system drives) #:select (drives:file-systems
                                                       drives:mapped-devices))

  #:export (feature-drives))

(define (feature-drives drives)
  (feature-file-systems
   #:mapped-devices
   (drives:mapped-devices drives)
   #:file-systems
   (drives:file-systems drives)))
