(define-module (x-files tests utils base overriding module-2)
  #:use-module ((x-files utils base)
                #:select (variable-rewire!))
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test)
  #:use-module (x-files tests utils base overriding module-1)
  #:export (g))

(define (g x)
  (* 2 (f x)))

(define module-1
  '(x-files tests utils base overriding module-1))

(define-test variable-rewiring-1
  (test-group "Call module-1 from module-2 (not overrided)"
    (test-equal (g 10) 22)
    (test-equal (f 3) 4)))

(variable-rewire! module-1 'a 2)

(define-test variable-rewiring-2
  (test-group "Call module-1 from module-2 (overrided)"
    (test-equal (f 3) 5)
    (test-equal (@@ (x-files tests utils base overriding module-1) a) 2)
    (test-equal (g 10) 24)))
