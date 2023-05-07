(define-module (x-files utils gexp)

  #:use-module ((guix build utils)
                #:prefix utils:)

  #:use-module (ice-9 match)

  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix tests)

  #:export (invoke))

(define* (invoke x . args)
  "Builds and invokes a gexp with `'args`'"
  (let* ((c (open-connection))
         (drv (run-with-store c (gexp->script "script" x))))
    (build-derivations c (list drv))
    (apply utils:invoke (derivation->output-path drv) args)))
