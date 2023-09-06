(define-module (x-files tests utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)

  #:use-module (x-files utils project)

  #:use-module (x-files utils tests)

  #:use-module (guix build utils)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext)
  #:use-module (srfi srfi-69)

  #:export (%test-dir
            with-test-dir
            run!))

(define %test-dir
  ;; Using project-local directory for tests
  (string-append (git-project-dir ".") "/tmp"))

(define-syntax-rule (with-directory-excursion* dir init body ...)
  "Run BODY with DIR as the process's current directory.
   Stolen from (guix build utils) except for setting custom `'init`' dir"
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
    (unless (directory-exists? d)
      (mkdir-p d))
    (with-directory-excursion*
     d (git-project-dir)
     (body d))))


(define (run!)
  (run-project-tests-cli
   (list
    (resolve-module '(x-files tests utils alist)))))
