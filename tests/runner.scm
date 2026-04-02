(use-modules
 (ares suitbl)
 (guix modules)
 (guix build utils))

(define files
  (let ((prefix "./tests/"))
    (map (lambda (x) (string-drop x (string-length prefix)))
         (find-files (string-append prefix "x-files")))))

(define modules
  (map (compose resolve-module file-name->module-name) files))

(define (module-public-test-suites module)
  (filter identity
          (module-map
           (lambda (k v)
             (and (variable-bound? v)
                  (let ((val (variable-ref v)))
                    (and (procedure? val)
                         (procedure-property val 'suitbl-test-suite?)
                         val))))
           (module-public-interface module))))

(define (run!)
  (let* ((runner (make-suitbl-test-runner))
         (suites (apply append (map module-public-test-suites modules)))
         (root  (test-suite-thunk "x-files"
                  (for-each (lambda (s) (s)) suites))))
    (run-test-suites runner (list root))
    (let ((summary (runner '((type . get-run-summary)))))
      (format #t "\n~a\n" summary)
      (when (> (+ (assoc-ref summary 'failures)
                  (assoc-ref summary 'errors))
               0)
        (exit 1)))))

(run!)
