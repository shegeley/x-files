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
  (let* [(tab* (apply string-append (make-list level tab)))
         (newl (if (eq? level 0) "" "\n"))
         (tabs (string-append newl tab*))]
    (call-with-values
      (lambda () (span (lambda (x) (not (list? x))) params))
      (lambda (a b)
        (let* [(subtables     (map (cut ->xtable <>
                                        #:level (+ level 1)
                                        #:start start
                                        #:end end) b))
               (subtables-str (apply string-append subtables))
               (rules-str     (string-append " " start
                                             subtables-str
                                             "\n" tab* end))
               (content      (match b
                               (() "")
                               (else rules-str)))]
          (string-append tabs (interpose a) content))))))

#|
(->xtable '("table" "ip" "filter"
            ("chain" "output"
             ("type" "filter")
             ("iffname"))
            ("chain" "forward")))
|#
