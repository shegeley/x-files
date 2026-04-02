(define-module (x-files tests utils alist)
  #:use-module (x-files utils alist)
  #:use-module (ares suitbl)
  #:use-module (srfi srfi-1))

(define alist*
  `((a . b)
    (hello . ((world . #t)
              (its-me . #f)))
    (animals . ((feline . ((cats . good)))
                (canine . ((wolfs . bad)
                           (dogs . good)))))))

(define-test-suite match-alist-tests
  (test "simple key lookup"
    (is (equal? 'b (match-alist alist* (('(a) a)) a)))
    (is (equal? 'b (match-alist alist* (a) a))))

  (test "nested path lookup"
    (is (equal? #t (match-alist alist* (('(hello world) hw)) hw)))
    (is (equal? #f (match-alist alist* (('(hello its-me) hw)) hw))))

  (test "multiple bindings"
    (is (equal? '(good bad good)
          (match-alist alist*
                       (('(animals canine dogs) dogs)
                        ('(animals canine wolfs) wolfs)
                        ('(animals feline cats) cats))
                       (list dogs wolfs cats)))))

  (test "missing key returns #f"
    (is (equal? #f (match-alist alist* (lol) lol)))))
