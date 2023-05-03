(define-module (x-files utils tests)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)

  #:use-module (x-files utils project)
  #:use-module (guix build utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-69)

  #:export (results-formatter
            test-runner*
            define~test-runner
            %test-dir
            with-test-dir)

  #:export-syntax (define~))

(define (dev?)
  (equal? (getenv "ENV") "DEV"))

(define* (result-formatter
          port
          args)
  (match args
    ((or (list expected actual ok)
         (list ... expected actual ok))
     (format port
             "Expected value: ~a. Actual value: ~a. Test ~a ~%" expected actual
             (if ok "passed" "failed")))))

(define* (test-runner*
          #:key (port (if (dev?) #t #f)))
  (let ((runner (test-runner-null))
        ;; results := '((expected1 actual1 (equal? expected1 actual1))
        ;;              (expected2 actual2 (equal? expected2 actual2)))
        (results '()))
    (test-runner-on-test-end! runner
      (lambda (r)
        (begin
          (set!
           results
           (cons
            (list
             (test-result-ref runner 'expected-value)
             (test-result-ref runner 'actual-value)
             (case (test-result-kind runner)
               ((pass xpass) #t)
               ((fail xfail) #f)
               (else #t)))
            results)))))
    (test-runner-on-group-begin! runner
      (lambda (r name c)
        (format port "Start testing group ~a ... ~%" name)))
    (test-runner-on-group-end! runner
      (lambda (r)
        (format port "Done testing group ~%")))
    (test-runner-on-final! runner
      (lambda (r)
        (begin
          (format port "Done testing all ~%")
          (map (lambda (res)
                 (result-formatter port res))
               results))))
    runner))

(define* (define~test-runner
           name
           proc
           example
           #:key (port (if (dev?) #t #f)))
  (let ((runner (test-runner-null))
        ;; results := '((expected1 actual1 (equal? expected1 actual1))
        ;;              (expected2 actual2 (equal? expected2 actual2)))
        (results '()))
    (test-runner-on-test-begin! runner
      (lambda (runner)
        (format port "Start testing ~a. ~%" name)))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (begin
          (set!
           results
           (cons
            (list
             (match example ((args . result) args))
             (test-result-ref runner 'expected-value)
             (test-result-ref runner 'actual-value)
             (case (test-result-kind runner)
               ((pass xpass) #t)
               ((fail xfail) #f)
               (else #t)))
            results)))))
    (test-runner-on-final! runner
      (lambda (runner)
        (begin
          (format port
                  "Done testing ~a. ~%"
                  (symbol->string (procedure-name proc)))
          (map (lambda (res)
                 (result-formatter port res))
               results))))
    runner))

(define* (test!
          proc
          examples)
  (for-each
   (lambda (example i)
     (match example
       ((args . result)
        (begin
          (let ((args (if (list? args) args (list args)))
                ;; "upper one" is just for convinience to be able to write '((2 . 4) (4 . 8)), not '(((2) . 4) (((4) . 8)))
                (name (format #f "Test №~a of *~a* with arguments <~a> and expected result <~a>. ~%"
                              i (procedure-name proc) args result)))
            (test-runner-factory
             (lambda ()
               (define~test-runner name proc example)))
            (test-begin name)
            (test-equal (apply proc args) result)
            (test-end)
            ;; resetting the test-runner to default
            (test-runner-factory
             (lambda ()
               (test-runner-simple))))))))
   examples (iota (+ 1 (length examples)) 1 1)))

(define-syntax define~
  ;; small utility-macros to instatly test pure functions on their definition
  (syntax-rules ()
    ;; examples := '((args1 . result1)
    ;;               (args2 . result2))
    ((_ (pname . args) examples body)
     (begin
       (define pname (lambda* args body))
       (test! pname examples)))
    ((_ pname examples body)
     (begin
       (define* pname body)
       (test! pname examples)))))

;; (define~ (thrise x)
;;   `(((2) . 6)
;;     ((4) . 12)
;;     ((6) . 18))
;;   (* 3 x))

;; (define~ twice
;;   `((2 . 4)
;;     (4 . 8)
;;     (6 . 12))
;;   (lambda (x) (* 2 x)))


(define %test-dir
  (string-append (git-project-dir)
                 "/tmp"))


(define-syntax-rule (with-directory-excursion* dir init body ...)
  "Run BODY with DIR as the process's current directory."
  (let ((init* (or init (getcwd))))
    (dynamic-wind
      (lambda ()
        (chdir dir))
      (lambda ()
        body ...)
      (lambda ()
        (chdir init*)))))

(define (with-test-dir dir body)
  ;; body := procedure of one argument (absolutepath in the %test-dir)
  (let ((d (string-append %test-dir "/" dir)))
    (with-directory-excursion*
     d (git-project-dir)
     (body d))))
