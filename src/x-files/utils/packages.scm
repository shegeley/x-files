(define-module (x-files utils packages)

  #:use-module (srfi srfi-1)

  #:export (specification->package-name))

(define (specification->package-name x)
  (list->string
   (take-while
    (lambda (x)
      (not (char-set-contains?
            (char-set #\: #\@)
            x))) (string->list x))))
