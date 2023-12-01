(define-module (x-files tests utils git)
  #:use-module (x-files utils git)
  #:use-module (x-files utils tests)

  #:use-module (ice-9 peg)

  #:use-module (srfi srfi-64-ext test))

(define example
  "git@github.com:guile-wayland/guile-wayland.git")

(define-test git-uri-peg-parsing
  (test-group "git-uri-peg-parsing"
    (test-equal '() (parse-git-url example))))
