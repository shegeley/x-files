(define-module (x-files tests utils records)
  #:use-module (x-files utils records)
  #:use-module (ares suitbl))

(define-test-suite define-record-type!-tests
  (test "constructor, predicate, and getters"
    (define-record-type! person
      (name)
      (age (default 27)))

    (define i
      (person
       (name "Ivan")
       (age 30)))

    (is (person? i))
    (is (equal? 30 (person:age i)))
    (is (equal? "Ivan" (person:name i))))

  (test "default field value"
    (define-record-type! point
      (x (default 0))
      (y (default 0)))

    (define p (point))
    (is (equal? 0 (point:x p)))
    (is (equal? 0 (point:y p)))))
