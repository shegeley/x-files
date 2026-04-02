(define-module (x-files tests utils base overriding module-2)
  #:use-module ((x-files utils base)
                #:select (with-variable-rewire))
  #:use-module (ares suitbl)
  #:use-module (x-files tests utils base overriding module-1)
  #:re-export (f)
  #:export (g))

(define (g x)
  (* 2 (f x)))

(define module-1
  '(x-files tests utils base overriding module-1))

(define m
  (resolve-module module-1))

(define-test-suite variable-rewiring-tests
  (test "with-variable-rewire temporarily overrides binding"
    (with-variable-rewire module-1 'a 2
      (is (equal? 2 (module-ref m 'a)))
      (is (equal? 5 (f 3)))
      (is (equal? 24 (g 10))))
    (is (equal? 1 (module-ref m 'a)))))
