(define-module (x-files utils string)
  #:use-module (ice-9 regex)

  #:export (string-split-regexp))

(define* (string-split-regexp
          string regex
          #:optional (uniq #\!))
  "Splits @code{string} by @code{regexp}.
   NOTE: needs rewrite. Current implementation is very inefficient, relies on inserting single @code{uniq} character that shouldn't be in the @code{string} beforehand"
  (string-split
   (regexp-substitute/global
    #f regex string
    'pre (char-set->string (char-set uniq)) 'post) uniq))
