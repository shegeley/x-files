(define-module (x-files tests utils base overriding module-3)
  #:use-module ((x-files utils base)
                #:select (variable-rewire!))
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test)
  #:use-module (x-files tests utils base overriding module-2))

(define-test variable-rewiring-2
  (test-group "Call module-2 with overrided module-1 from module-3"
    (test-equal 5 (f 3))
    (test-equal 24 (g 10))))
