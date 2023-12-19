(define-module (x-files file-system file)
  #:use-module (guix records)

  #:export (file))

(define* (file file-system relative-path)
  "Simple wrapper to handle all filesystem reference. Return string. Can be hooked to check if the reference exists and create it on the fly"
  (match-record file-system (@@ (gnu system file-systems) <file-system>)
    (mount-point)
    (string-append mount-point relative-path)))
