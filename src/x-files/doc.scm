(define-module (x-files doc)
  #:use-module (x-files utils base)
  #:use-module (x-files modules)
  #:use-module (x-files utils project)
  #:use-module (guix build utils)
  #:use-module (gnu services shepherd)

  #:use-module (texinfo reflection)
  #:use-module (texinfo serialize)
  #:use-module (texinfo html)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 documentation)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)

  #:use-module (guix packages)
  #:use-module (gnu services)

  #:use-module (sxml simple)

  #:use-module (oop goops)

  #:export (generate-documentation
            export-documentation
            %exporters))

(define <package> (@@ (guix packages) <package>))

(define <service-type> (@@ (gnu services) <service-type>))

(define <shepherd-service> (@@ (gnu services shepherd) <shepherd-service>))

(define %doc-resolvers
  (make-parameter
   `((,package? . ,package-description)
     (,service-type? . ,service-type-description)
     (,shepherd-service? . ,shepherd-service-documentation))))

(define %default-formatter
  (make-parameter
   (lambda (name object)
     `(defspec (% (name ,(symbol->string name)))
        (para ,(object-documentation object))))))

(define %try-set-documentation
  (make-parameter
   (lambda (object)
     (let* [(pair (find
                   (match-lambda
                     ((p . d) (p object)))
                   (%doc-resolvers)))
            (documentation (if pair ((cdr pair) object) #f))]
       (if documentation
           (begin (set-object-property! object 'documentation documentation) #t)
           #f)))))

(define* (resolver module-name)
  (lambda (name def)
    (let* [(module* (resolve-module module-name))
           (var (module-variable module* name))
           (object (and (variable-bound? var)
                        (variable-ref var)))
           (documentation ((%try-set-documentation) object))]
      (if documentation
          ((%default-formatter) name object)
          def))))


(variable-rewire!
 `(texinfo reflection) 'module-stexi-documentation
 (lambda (module-name)
   (module-stexi-documentation module-name #:docs-resolver (resolver module-name))))

(define doc:dir
  (string-append (git-project-dir ".") "/doc"))

(define (generate-documentation modules)
  (package-stexi-documentation
   modules "x-files:doc" "" '() '()))

(define %exporters
  (make-parameter
   `((info . ,(lambda (doc p) (display (stexi->texi doc) p)))
     (html . ,(lambda (doc p) (sxml->xml (stexi->shtml doc) p))))))

(define (export-documentation documentation format)
  "@code{documentation} is an @code{stexi} tree. @code{format} is 'html or 'info"
  (let [(exporter (assoc-ref (%exporters) format))]
    (mkdir-p doc:dir)
    (call-with-output-file
        (string-append
         doc:dir "/x-files"
         "." (symbol->string format))
      (lambda (p) (exporter documentation p)))))
