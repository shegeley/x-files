(define-module (x-files tests storage)
  #:use-module (x-files storage)
  #:use-module (x-files utils tests)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test))

(define-test git-subdir-storage
  (test-group "git-subdir-storage"
    (with-test-dir
     "storage"
     (lambda (d)
       (let* [(test-file (string-append (%test-dir) "/file.txt"))
              (_ (with-output-to-file test-file
                   (lambda () (display "test"))))
              (storage (init <git-subdir-storage> d))
              (path `(a b.txt))]
         (add storage test-file path #f)
         (test-equal
             (string-append d "/a/b.txt")
             (get storage path '())))))))
