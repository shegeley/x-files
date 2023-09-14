(define-module (x-files tests utils base overriding module-2)
  #:use-module ((x-files utils base)
                #:select (variable-rewire!))
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

(define-test variable-rewiring-1:1
  (test-group
      "Call module-1 from module-2. 'Before' override.
       Would be already overrided (seems module & variable
       resolving is the first thing guile does)"
    (test-equal 2 (module-ref m 'a))
    (test-equal 5 (f 3))
    (test-equal 24 (g 10))))

(variable-rewire! module-1 'a 2)

(define-test variable-rewiring-1:2
  (test-group "Call module-1 from module-2. 'After' override."
    (test-equal 2 (module-ref m 'a))
    ;; NOTE: those 2 cases below won't run properly in srfi-64-ext environment; because of how it resolves modules
    (test-equal 5 (f 3))
    (test-equal 24 (g 10))))
