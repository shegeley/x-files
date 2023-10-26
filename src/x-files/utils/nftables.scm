(define-module (x-files utils nftables)
  #:use-module (x-files utils base)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:export (->xtable))

(define* (->xtable params
                   #:key
                   (level 0)
                   (tab "\t")
                   (start "{")
                   (end "}"))
  "@code{params} expected to be (ip/nf)tables tree-like data structure like:
   @example
    '(\"table\" \"ip\" \"filter\"
        (\"chain\" \"output\"
        (\"type\" \"filter\" ...)
        (\"iffname\" ... ...))
        (\"chain\" \"forward\"
        (...) ...))
   @end example
   @code{level} is a base identation level (whole number)
   @code{tab} is a tabulation/spacing symbol
   @code{start} and @code{end} are form opening/closing symbols respectfully"
  (let* ((tab* (apply string-append (make-list level tab))))
    (call-with-values
        (lambda () (span (lambda (x) (not (list? x))) params))
      (lambda (a b)
        (string-append
         (string-append "\n" tab*)
         (interpose a)
         (match b
           (() "")
           (else
            (string-append
             " " start
             (apply
              string-append
              (map
               (cut ->xtable <>
                    #:level (+ level 1)
                    #:start start
                    #:end end) b))
             "\n" tab* end))))))))
