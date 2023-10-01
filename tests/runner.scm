(use-modules
 (srfi srfi-64-ext)
 (guix modules)
 (guix build utils))

(define files
  (let ((prefix "./tests/"))
    (map (lambda (x)  (string-drop x (string-length prefix)))
         (find-files (string-append prefix "x-files")))))

(define modules
  (map (compose resolve-module file-name->module-name) files))

(define (run!)
  (run-project-tests-cli modules))

(run!)
