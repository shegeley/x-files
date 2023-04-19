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

(define~ (ref-in x path)
  `(((((a . 1)
       (b . 2)
       (c . ((c1 . 11)
             (c2 . 12)))) (c c2)) . 12)
    ;; NOTE: be careful when dealing with #t #f as values
    ((((a . 1)
       (b . 2)
       (c . ((c1 . 11)
             (c2 . 12)))) (c c3)) . #f)
    ((((a . #f)
       (b . 2)
       (c . ((c1 . 11)
             (c2 . #f)))) (a)) . #f))
  (match path
    ('() x)
    (else
     (ref-in (assoc-ref x (first path))
             (cdr path)))))

(define* (hash-unique l
                      #:key (f (lambda (x y) x)))
  (let ((h (make-hash-table (length l))))
    (begin
      (map
       (lambda (x)
         (hash-set! h x #t)) l)
      (hash-map->list f h))))

(define* (pairize f x #:optional
                  (g identity)
                  (h identity))
  (h (cons (g x) (f x))))
