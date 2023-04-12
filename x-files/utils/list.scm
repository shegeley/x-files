(define-module (x-files utils list)
  #:use-module (x-files utils tests)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (replace-by
            list->index-assoc-list
            alist->even-list
            even-list->alist))

(define even-list->alist
  (match-lambda
    ((a)
     (error "Not even list"))
    ((a b)
     `((,a . ,b)))
    ((a b c ...)
     (alist-cons a b
                 (even-list->alist c)))))

(define alist->even-list
  (match-lambda
    ('()
     '())
    (((a . b) rest ...)
     (append (list a b)
             (alist->even-list rest)))))

(define (list->index-assoc-list lst)
  (map (lambda (x i)
         (cons i x))
       lst
       (iota (length lst))))

(define~ (replace-by xs pred copred)
  `((((-2 1 2 3 4) ,even? ,(lambda (x) (+ 10 x)))
     . (8 1 12 3 14)))
  (match xs
    ('()
     '())
    ((x1 rest ...)
     (if (pred x1)
         (cons* (copred x1) (replace-by rest pred copred))
         (cons* x1 (replace-by rest pred copred))))))


(define (even-list-replace elist x f)
  (let* ((alist (even-list->alist elist))
         (prev-value
          (assoc-ref (even-list->alist elist) x)))
    (alist->even-list
     (assoc-set! alist x (f prev-value)))))
