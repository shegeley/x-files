(define-module (x-files doc)
  #:use-module (guix modules)
  #:use-module (guix build utils))

(define files
  (let ((prefix "./src/"))
    (map (lambda (x)  (string-drop x (string-length prefix)))
         (find-files (string-append prefix "x-files")))))

(define modules
  (map (compose resolve-module file-name->module-name) files))

(display modules)
