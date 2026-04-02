(define-module (x-files tests storage)
  #:use-module (x-files storage)
  #:use-module (x-files utils tests)
  #:use-module (ares suitbl)
  #:use-module (srfi srfi-1))

(define-test-suite git-subdir-storage-tests
  (test "add and get a file"
    (with-test-dir
     "storage"
     (lambda (d)
       (let* ((test-file (string-append (%test-dir) "/file.txt"))
              (_ (with-output-to-file test-file
                   (lambda () (display "test"))))
              (storage (init <git-subdir> d))
              (path '(a b.txt)))
         (add storage test-file path #f)
         (is (equal? (string-append d "/a/b.txt")
                     (get storage path '()))))))))
