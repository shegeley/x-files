(define-module (x-files utils gexp)

  #:use-module ((guix build utils)
                #:prefix utils:)

  #:use-module (ice-9 match)

  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix tests)

  #:export (invoke
            build
            local-file*))

(define (build g)
  (let* ((c (open-connection))
         (drv (run-with-store c (gexp->script "script" g))))
    (build-derivations c (list drv))
    (derivation->output-path drv )))

(define* (invoke x . args)
  "Builds and invokes a gexp with `'args`'"
  (let* ((r (build x)))
    (apply utils:invoke r args)))

(define (gen-name x)
  (let ((b (basename x)))
    (if (equal? "." (string-take b 1))
        (string-drop b 1)
        b)))

(define* local-file*
  (lambda (x)
    (local-file
     x (gen-name x)
     #:recursive?
     (if (directory-exists? x)
         #t #f))))
