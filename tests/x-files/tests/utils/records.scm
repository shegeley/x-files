(define-module (x-files tests utils records)
  #:use-module (x-files utils records)
  #:use-module (x-files utils tests)

  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test))

(define-test define-record-type-
  (test-group "define-record-type!"
    (define-record-type! person
      (name)
      (age (default 27)))

    (define i
      (person
       (name "Ivan")
       (age 30)))

    (test-equal #t (person? i))

    (test-equal 30 (person:age i))

    (test-equal "Ivan" (person:name i))))
