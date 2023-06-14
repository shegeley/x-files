(define-module (x-files utils records)
  #:use-module (guix records)

  #:export-syntax (define-record-type!))

(define-syntax-rule (id ctx parts ...)
  "NOTE: Stolen from (gnu services configuration)
   NOTE: Coudn't refer via (@@ (gnu services configuration) id) or modules #:select
   DOC: Assemble PARTS into a raw (unhygienic) identifier."
  (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))

(define-syntax define-record-type!
  (lambda (s)
    "I just don't want to write all the <record> record make-record and stuff all over again. Also I want to have juiist record:field getters generated by-default.
    Example:

    (define-record-type! person
        (name (default \"Ivan\"))
        (age (default 27)))

    (define i
      (person ff
       (name \"Ivan\")
       (age 30)))
    (person? i) => #t
    (person:age i) => 30
    (person:name i) => \"Ivan\""

    ;; NOTE: original syntax for define-record-type* is :
    ;; (_ type ;; <thing>
    ;;    syntactic-ctor ;; thing
    ;;    ctor ;; make-thing
    ;;    pred ;; thing?
    ;;    this-identifier ;; this-thing
    ;;    (field get properties ...) ...)

    (syntax-case s ()
      [(_ record:type:name
          (field properties ...) ...)
       #`(define-record-type*
           #,(id #'record:type:name #'< #'record:type:name #'>)
           record:type:name
           #,(id #'record:type:name #'record:type:name #'!)
           #,(id #'record:type:name #'record:type:name  #'?)
           #,@(map
               (lambda (name properties)
                 #`(#,name
                    #,(id #'record:type:name
                          #'record:type:name
                          #':
                          name)
                    #,@properties))
               #'(field ...)
               #'((properties ...) ...)))])))



(define-record-type! person
  (name)
  (age (default 27)))

(define i
  (person
   (name "Ivan")
   (age 30)))

(person? i)

(person:name i)

(person:age i)