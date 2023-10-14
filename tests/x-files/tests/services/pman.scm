(define-module (x-files tests services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (gnu services mcron)
  #:use-module (gnu packages guile-xyz)

  #:use-module (gnu home services)
  #:use-module (git remote)

  #:use-module (x-files services pman)
  #:use-module (x-files utils tests)
  #:use-module (x-files utils git)
  #:use-module (x-files utils tests)
  #:use-module (x-files utils project)
  #:use-module ((x-files utils gexp)
                #:prefix g:)
  #:use-module (guix build utils)

  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix tests)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test))

(define project-dir "notes")

(define %keys
  (make-parameter
   (list (string-append (getenv "HOME")
                        "/.ssh/main"))))
(define %projects
  (make-parameter
   (list (project*
          ;; NOTE: Here you should have your private repo to test
          "git@gitlab.com:shegeley/notes.git"
          project-dir))))

(define-test pman
  (test-group "pman"
    (with-test-dir
     "pman"
     (lambda (d)
       (let* ((%manager
               (project-manager-conf
                (dir d)
                (period 2)
                (keys (%keys))
                (projects (%projects))))
              (%project (first (%projects)))
              (d** (string-append d "/"
                                  (project:dir %project))))
         (g:invoke
          (with-ssh-agent
           (project-manager:keys %manager)
           (map (lambda (project) (g-clone! %manager project)) (%projects))))
         (test-equal #t (directory-exists? d**))
         (test-assert
             (member (project:source %project)
                     (map remote-url (remotes d**))))
         (let* [(timestamp (lambda () (stat:ctime (stat (string-append d** "/.git/FETCH_HEAD")))))
                (t1 (timestamp))]
           (g:invoke
            (with-ssh-agent (project-manager:keys %manager)
                            (list (g-fetch! %manager %project))))
           (test-assert (> (timestamp) t1))
           ;; (let* [(files ((@@ (gnu services mcron) job-files) mcron
           ;;                (list (with-modules+exts
           ;;                       #~(job (lambda (t) (+ t 2))
           ;;                              #$fetcher-file
           ;;                              "Project manager's fetcher daemon")))))
           ;;        (file (first files))
           ;;        (gexp* #~(system* #$(file-append mcron "/bin/mcron") #$file))]
           ;;   (g:build gexp*)
           ;;   (g:invoke gexp*)
           ;;   )
           ))))))
