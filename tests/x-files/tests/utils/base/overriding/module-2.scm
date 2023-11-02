(define-module (x-files tests utils base overriding module-2)
  #:use-module ((x-files utils base)
                #:select (with-variable-rewire))
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test)
  #:use-module (x-files tests utils base overriding module-1)
  #:re-export (f)
  #:export (g))

(define (g x)
  (* 2 (f x)))

(define module-1
  '(x-files tests utils base overriding module-1))

(define m
  (resolve-module module-1))

(define-test variable-rewiring
  (test-group "Call module-1 from module-2 with rewireing"
    (with-variable-rewire module-1 'a 2
     (test-equal 2 (module-ref m 'a))
     ;; NOTE: those 2 cases below won't run properly in srfi-64-ext environment; because of how it resolves modules
     (test-equal 5 (f 3))
     (test-equal 24 (g 10)))
    (test-equal 1 (module-ref m 'a))))
