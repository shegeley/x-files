(define-module (x-files tests services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (gnu home services)

  #:use-module (x-files services pman)
  #:use-module (x-files utils tests)
  #:use-module (x-files utils git)
  #:use-module (x-files utils project)
  #:use-module ((x-files utils gexp)
                #:prefix g:)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (git auth)
  #:use-module (git clone)

  #:use-module (guix build utils)

  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (guix monad-repl)
  #:use-module (guix store)
  #:use-module (guix tests)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 exceptions)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))


(setenv "ENV" "DEV")

(test-runner-factory
 (lambda () (test-runner*)))

(with-test-dir
 "pman"
 (lambda (d)
   (let* ((project-dir "notes")
          (%project
           (project*
            ;; NOTE: Here you should have your private repo to test
            "git@gitlab.com:shegeley/notes.git"
            project-dir))
          (%manager
           (project-manager-conf
            (dir d)
            (projects (list %project))))
          (d** (string-append d "/"
                              (project:dir %project))))
     (test-begin "git clone private repo: as a simple operation from pman module")
     (g:invoke ((@@ (x-files services pman) template)
                %manager
                (@@ (x-files services pman)
                    g-clone!)))
     (test-equal #t (directory-exists? d))
     (test-assert
         (member (project:source %project)
                 (map remote-url (remotes d**))))
     (test-end "git clone private repo: as a simple operation from pman module"))))
