(define-module (x-files tests services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (gnu home services)

  #:use-module (x-files services pman)
  #:use-module (x-files utils tests)
  #:use-module (x-files utils git)
  #:use-module (x-files utils project)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (git auth)
  #:use-module (git clone)

  #:use-module (guix build utils)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (guix store)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 exceptions)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define %project
  (project*
   ;; NOTE: Here you should have your private repo to test
   "git@gitlab.com:shegeley/notes.git"
   "notes"))

(setenv "ENV" "DEV")

(test-runner-factory
 (lambda () (test-runner*)))

(with-test-dir
 "pman"
 (lambda (d)
   (let ((%manager
          (project-manager-conf
           (dir d)
           (projects (list %project)))))
     (test-begin "git clone private repo: as a simple operation from pman module")
     (test-assert
         ((@@ (x-files services pman)
              clone!)
          ((@@ (x-files services pman) project:source)
           %project)
          (string-append d "/my-notes")
          (%make-auth-ssh-agent)))
     (test-end)
     (test-begin "git clone private repo: as gexp from pman module")
     (test-assert
         ;; NOTE: simple test-assert of error catching (makes sure there is no errors)
         (run-with-store (open-connection)
           (gexp->script "pman/clone"
                         ((@@ (x-files services pman)
                              g-clone!) %manager %project))))
     (test-end))))
