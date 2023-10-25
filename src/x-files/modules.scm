(define-module (x-files modules)
  #:use-module (guix modules)
  #:use-module (guix build utils)
  #:use-module (guix packages)

  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-13)

  #:use-module (ice-9 match)

  #:use-module (x-files utils project)
  #:use-module (geiser doc)

  #:export (%classifiers
            modules
            classify-module-object
            module-entities))

(define %classifiers
  (make-parameter
   (list
    package?
    shepherd-service?
    service-type?
    procedure?)))

(define files
  (let* [(prefix "/src/")
         (absolute-path (string-append (git-project-dir ".") prefix))]
    (map (lambda (x) (string-drop x (string-length absolute-path)))
         (find-files absolute-path))))

(define modules
  (map (compose resolve-module file-name->module-name) files))

(define* (classify-module-object name var elts)
  "Applies @code{classifier} on @code{var}. Returns list of three (@code{type} @code{name} @code{obj}) (predicate from @code{%classifiers}, object's (var) symbolic name, and the @code{obj} itself)"
  (let* [(obj (and (variable-bound? var)
                   (variable-ref var)))]
    (when obj (let* [(classifier
                      (find
                       (lambda (p) (p obj))
                       (%classifiers)))]
                (when classifier (list classifier name obj))))))

(define (module-entities module)
  "Returns hash-table of module entities. Key is an element of @code{%classifiers} and values are all module's obj and names pairs (@code{name} @code{obj}) (symbol, object)"
  (let* [(entities (make-hash-table))]
    (hash-fold
     (lambda args
       (false-if-exception
        (match-let* [((type name obj)
                      (apply classify-module-object args))]
          (hash-set!
           entities type
           (cons (cons name obj)
                 (or (hash-ref entities type)
                     '()))))))
     (list '() '() '())
     (module-obarray module))
    entities))
