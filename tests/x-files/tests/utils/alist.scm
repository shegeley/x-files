(define-module (x-files tests utils alist)
  #:use-module (x-files utils alist)
  #:use-module (x-files tests utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(test-runner-factory
 (lambda ()
   (test-runner*)))

(test-begin "match-alist")

(define alist*
  `((a . b)
    (hello . ((world . #t)
              (its-me . #f)))
    (animals . ((feline . ((cats . good)))
                (canine . ((wolfs . bad)
                           (dogs . good)))))))

(test-equal 'b
  (match-alist alist* (('(a) a)) a))

(test-equal 'b
  (match-alist alist* (a) a))

(test-equal #t
  (match-alist alist*
               (('(hello world) hw))
               hw))

(test-equal #f
  (match-alist alist*
               (('(hello its-me) hw))
               hw))

(test-equal `(good bad good)
  (match-alist alist*
               (('(animals canine dogs) dogs)
                ('(animals canine wolfs) wolfs)
                ('(animals feline cats) cats))
               (list dogs wolfs cats)))

(test-equal #f
  (match-alist alist* (lol) lol))

(test-end "match-alist")
