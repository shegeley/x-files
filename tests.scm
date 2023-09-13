(use-modules
 (srfi srfi-64-ext))

(define (run!)
  (run-project-tests-cli
   (list
    (resolve-module '(x-files tests utils base overriding module-2))
    (resolve-module '(x-files tests utils base overriding module-3))
    (resolve-module '(x-files tests utils records))
    (resolve-module '(x-files tests utils alist))
    (resolve-module '(x-files tests utils base)))))

(run!)
