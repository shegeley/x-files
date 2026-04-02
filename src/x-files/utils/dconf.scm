(define-module (x-files utils dconf)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:export (merge-dconf-entries))

(define (merge-section-kvs kvs1 kvs2)
  "Merge two key-value alists. kvs2 entries override kvs1 on key conflict."
  (fold (lambda (kv acc)
          (let ((existing (assoc (car kv) acc)))
            (if existing
                (map (lambda (k) (if (equal? (car k) (car kv)) kv k)) acc)
                (append acc (list kv)))))
        kvs1 kvs2))

(define (static-merge-dconf-entries entries1 entries2)
  (fold (lambda (section-entry result)
          (let* ((section (car section-entry))
                 (kvs (cdr section-entry))
                 (existing (assoc section result)))
            (if existing
                (map (lambda (e)
                       (if (equal? (car e) section)
                           (cons section (merge-section-kvs (cdr e) kvs))
                           e))
                     result)
                (append result (list section-entry)))))
        entries1 entries2))

(define (merge-dconf-entries entries1 entries2)
  "Merge two dconf entry alists.

   When both inputs are plain lists: sections present in both have their
   key-value pairs deep-merged (entries2 overrides entries1 on key conflict).

   When either input is a gexp: returns a gexp that appends both at build
   time.  Deep merging is not possible without evaluating the gexp, so
   duplicate section headers may appear in the output — dconf processes
   all key-value pairs regardless.

   Merging with an empty plain list is always a no-op and returns the other
   argument unchanged.  This avoids wrapping a gexp in an unnecessary
   #~(append #$gexp '()) which cannot be serialized by serialize-ini-gexp."
  (cond
    ((and (not (gexp? entries2)) (null? entries2)) entries1)
    ((and (not (gexp? entries1)) (null? entries1)) entries2)
    ((or (gexp? entries1) (gexp? entries2))
     #~(append #$entries1 #$entries2))
    (else
     (static-merge-dconf-entries entries1 entries2))))
