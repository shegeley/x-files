(define-module (x-files utils base)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 atomic)

  #:use-module (x-files utils tests)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-26)

  #:export (interpose
            variable-rewire!
            ref-in))

(define~ (interpose lst
                    #:optional (token " "))
  `(((("hello" "world")) . "hello world")
    ((("1" "2" "3") "-o-") . "1-o-2-o-3"))
  (match lst
    ('()
     "")
    ((a)
     a)
    ((a rest ...)
     (string-append a token (interpose rest token)))))

(define (variable-rewire! module-name var val)
  "«Rewires» a variable in module. Sets it to the val in current context (module/buffer/file).
   - Note: will only change values that are not «static».
    - Example:
        - (define x 1) (define-public y (+ x 10)). y won't be rewired.
        - (define x 1) (define-public (y) (+ x 10)). y will re rewired"
  (variable-set!
   (module-variable
    (resolve-module module-name) var) val))

(define (ref-in x path)
  (match path
    ('() x)
    (else
     (ref-in (assoc-ref x (first path))
             (cdr path)))))
