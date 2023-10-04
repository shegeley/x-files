(define-module (x-files utils hash-map)
  #:use-module (srfi srfi-13)

  #:export (hash-map:map))

(define (hash-map:map proc table)
  "Just hash-map map. @code{proc} takes arguments (key, value) and returns something"
  (let [(acc '())]
    (hash-for-each
     (lambda (x y) (set! acc (cons (proc x y) acc))) table) acc))
