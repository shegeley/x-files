(define-module (x-files packages databases)
  #:use-module (gnu)

  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)

  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system copy)

  #:use-module (nongnu packages mozilla))

;; https://downloads.mongodb.com/compass/mongosh-1.8.2-linux-x64.tgz

(define-public mongoshell
  (package
   (name "mongoshell")
   (version "1.8.2")
   (supported-systems
    '("x86_64-linux"))
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "https://downloads.mongodb.com/compass/mongosh-"
       version "-linux-x64.tgz"))
     (sha256
      (base32 "0mqwxnz4f1wkxwxjy3jf249acg05kbpyppx8nnq0lbkqspwn5yag"))))
   (build-system copy-build-system)
   (license license:asl2.0)
   (home-page "https://www.mongodb.com")
   (synopsis "MongoDB Shell is and official shell Mongo client")
   (description "MongoDB Shell is the quickest way to connect to (and work with) MongoDB. Easily query data, configure settings, and execute other actions with this modern, extensible command-line interface — replete with syntax highlighting, intelligent autocomplete, contextual help, and error messages..")))
