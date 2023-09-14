(define-module (x-files tests utils base overriding module-1)
  #:declarative? #f
  #:export (f a))

(define a 1)

(define (f x)
  (+ x a))
