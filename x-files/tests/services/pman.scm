(define-module (x-files tests services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (x-files utils git)

  #:use-module (x-files services pman)

  #:use-module (gnu home services)
  #:use-module (x-files utils project)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (git auth)
  #:use-module (git clone)

  #:use-module (guix build utils)

  #:use-module (guix gexp)
  #:use-module (guix records)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 exceptions)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define %dir
  (string-append (git-project-dir)
                 "/tmp"))

(define %projects
  (list
   (project*
    "git@gitlab.com:shegeley/notes.git"
    "notes")))

(define %project-manager
  (project-manager-conf
   (dir ())))

(test-begin "pman: clonning a private repo")

(test-end "pman: clonning a private repo")

(test-begin "pman: fetching a private repo")

(test-end "pman: fetching a private repo")
