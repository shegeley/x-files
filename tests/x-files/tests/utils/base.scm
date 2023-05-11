(define-module (x-files tests utils base)
  #:use-module (x-files tests utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(test-runner-factory
 (lambda ()
   (test-runner*)))

(define-module (module-1)
  #:export (f))

(define a 1)

(define (f x)
  (+ x a))

(define-module (module-2)
  #:use-module ((x-files utils base)
                #:select (variable-rewire!))
  #:use-module (srfi srfi-64)
  #:use-module (module-1)
  #:export (g))

(define (g x)
  (* 2 (f x)))

(test-begin "variable-rewiring")

(test-equal (g 10) 22)
(test-equal (f 3) 4)
(variable-rewire! '(module-1) 'a 2)

(test-equal (f 3) 5)
(test-equal (module-ref (resolve-module '(module-1)) 'a) 2) ;; Won't fail

(test-equal (g 10) 24)

(test-end)

(define-module (module-3)
  #:use-module ((x-files utils base)
                #:select (variable-rewire!))
  #:use-module (srfi srfi-64)
  #:use-module (module-2))

(test-begin "variable-rewiring")

(test-equal (@@ (module-1) a) 2)
(test-equal (g 10) 24)

(test-end)
