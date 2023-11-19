(define-module (x-files storage)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (x-files utils files)
  #:use-module (guix build utils)
  #:use-module (web uri)
  #:use-module (oop goops)

  #:export (<storage> <git-subdir>
            <local-directory>  init get add resolver))

(define-class <storage> ()
#| <storage> is a goops-class associated with entity that can store data and has some query-interface: file-system folder, database, current project's git subdirectory, IPFS and etc.


  The base <storage> class should have only `uri` and `resolver` fields.

  `uri` is the RFC 3986 uri of the storage itself and resolver is a procedure that will resolve query to it. example:

  @example
  (define-method (get (storage <git-subdir-storage>) path options)
   "Returns just the string (path) to the file in troage"
   ((resolver storage) path))
  @end example |#
  uri
  resolver)

(define-class <local-directory> (<storage>)
  (directory
   #:init-keyword #:directory
   #:getter directory)
  (uri
   #:init-keyword #:uri
   #:getter uri)
  (resolver
   #:init-keyword #:resolver
   #:getter resolver))

(define (init x directory)
  (match x
    (<local-directory>
     (let* [(directory* (canonicalize-path directory))
            (uri (string->uri (string-append "file://" (canonicalize-path directory))))
            (path (uri-path uri))]
       (make <local-directory>
         #:directory directory*
         #:uri uri
         #:resolver (lambda (path) (string-append directory* (symlist->fs-path path))))))))

(define-method (get (storage <local-directory>) path options)
  "Returns just the string (path) to the file in troage"
  (let [(path* ((resolver storage) path))]
    (if (file-exists? path*)
        path*
        (error (format #f "File ~a from path ~a does not exists in a storage.~%" path* path)))))

(define-method (add (storage <local-directory>) uri path options)
  "Uri is the source-uri. If no uri given, just the file-system path, it will append 'file://' automatically"
  (when (or (string? uri)
            (equal? 'file:// (uri-scheme uri)))
    (let [(source (cond ((uri? uri) (uri-path uri))
                        ((string? uri) uri)))
          (up ((resolver storage) (drop-right path 1)))
          (destination ((resolver storage) path))]
      (unless (and (not (equal? up '()))
                   (directory-exists? up))
        (mkdir-p up))
      (copy-recursively source destination))))

(define-class <git-subdir> (<local-directory>)
  (directory
   #:init-keyword #:directory
   #:getter directory)
  (uri
   #:init-keyword #:uri
   #:getter uri)
  (resolver
   #:init-keyword #:resolver
   #:getter resolver))


;; NOTE: add argument-checking with (system vm program)?

;; (define-method (move (storage <git-subdir-storage>) src dst)
;;   ())

;; (define-method (remove (storage <git-subdir-storage>) path))

;; (define-method (history (storage <storage>) path)
;;   ;; TODO realization
;;   #f)
