(define-module (x-files storage)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (x-files utils files)
  #:use-module (guix build utils)
  #:use-module (web uri)
  #:use-module (oop goops)

  #:export (<storage> <git-subdir-storage>
                      init get add))

(define-class <storage> ()
  uri
  resolver)

(define-class <git-subdir-storage> (<storage>)
  (directory #:getter directory)
  (uri #:getter uri)
  (resolver #:getter resolver))

(define (init x directory)
  (match x
    (<git-subdir-storage>
     (let* [(directory* (canonicalize-path directory))
            (uri (string->uri (string-append "file://" (canonicalize-path directory))))
            (path (uri-path uri))]
       (make <git-subdir-storage>
         #:directory directory*
         #:uri uri
         #:resolver (lambda (path) (string-append directory* (symlist->fs-path path))))))))

(define-method (get (storage <git-subdir-storage>) path options)
  "Returns just the string (path) to the file in troage"
  (resolver storage))

(define-method (add (storage <git-subdir-storage>) uri path options)
  "Uri is the source-uri. If no uri given, just the file-system path, it will append 'file://' automatically"
  (when (or (string? uri)
            (equal? 'file:// (uri-scheme uri)))
    (let [(source (cond ((uri? uri) (uri-path uri)
                         (string? uri) uri)))]
      (copy-recursively source (apply (resolver storage) path)))))

;; NOTE: add argument-checking with (system vm program)?

;; (define-method (move (storage <git-subdir-storage>) src dst)
;;   ())

;; (define-method (save (storage <git-subdir-storage>) port path)
;;   ())

;; (define-method (remove (storage <git-subdir-storage>) path))

;; (define-method (history (storage <storage>) path)
;;   ;; TODO realization
;;   #f)
