(define-module (x-files services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)
  #:use-module (x-files utils git)

  #:use-module (gnu packages gnupg)

  #:use-module (gnu services configuration)
  #:use-module (gnu home services mcron)

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (gnu packages guile)

  #:use-module (guix describe)
  #:use-module (guix gexp)
  #:use-module (guix git)
  #:use-module (guix channels)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix build utils)
  #:use-module (guix packages)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 exceptions)

  #:use-module (oop goops)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (<project-manager-conf>
            project-manager-conf
            <project>
            channel->project))

(define-record-type* <project> project make-project
  project?
  this-project
  (source project-source)
  (dir project-dir))

(define-public (project* s d)
  (make-project s d))

(define-record-type* <project-manager-conf>
  project-manager-conf
  project-manager-conf!
  project-manager-conf~
  project-manager-conf?
  (dir project-manager-conf:dir)
  (period project-manager-conf:period (default 300))
  ;; project can be a project* a channel or a package
  (projects project-manager-conf:projects))

(define* (template config body
                   #:key (wrapper identity))
  (match-record config <project-manager-conf>
                (projects (dir manager-dir))
                (wrapper
                 (with-extensions
                  (list guile-git
                        guile-bytestructures
                        guile-gcrypt)
                  (with-imported-modules
                   (source-module-closure
                    '((ice-9 format) (guix build utils)
                      (x-files utils git))
                    #:select?
                    (lambda (name)
                      (or ((@@ (guix modules) guix-module-name?) name)
                          (match name
                            (('x-files 'utils ..) #t)
                            (('git ..) #t)
                            (else #f)))))
                   #~(begin
                       (use-modules
                        (git bindings) (ice-9 format)
                        (guix build utils) (x-files utils git))

             #$@(map
                 (lambda (project)
                   (match-record project <project>
                     (source (dir project-dir))
                     (body manager-dir project))) projects)))))))

(define (fetcher manager-dir project)
  (match-record project <project>
    (source (dir project-dir))
    #~(begin
        (libgit2-init!)
        ;; TODO: add a way to fetch git with credentials
        (let* ((realdir (string-append #$manager-dir "/" #$project-dir)))
          (false-if-exception
           (begin
             (fetch-remotes realdir)
             (format #t "Git-repository ~a was successfully fetched. ~%" realdir))
               (lambda (ex)
                 (catch 'git-error
                   ;; NOTE: Needs to be handled properly. Having problem with guile git lib auth
                   (lambda ()
                     (format #t "Coudn't fetch ~a git-repo. ~%" realdir)
                     #f)
                   (lambda _ #f))))))))

(define (fetcher-program-file config)
  ;; TODO: create template from both fetcher and activatoion.
  ;; reduce code duplication
  (template config fetcher
            #:wrapper
            (lambda (x)
              (program-file
               "project-manager-fetcher-script.scm" x))))

(define (mcron-fetcher config)
  (match-record config <project-manager-conf>
    (period)
    (list #~(job (lambda (t) (+ t #$period))
                 #$(fetcher-program-file config)
                 "Project manager's fetcher daemon"))))

(define (channel->project channel)
  (match-record channel (@@ (guix channels) <channel>)
    (name url)
    (project* url (symbol->string name))))

(define (activation config)
  (template config
            (lambda (manager-dir project)
              (match-record
                  project <project>
                (source (dir project-dir))
                #~(begin
                    (let* ((realdir (string-append #$manager-dir "/" #$project-dir)))
                      (if (directory-exists? realdir)
                          (format #t "Directory ~s already exists. Skip cloning. ~%" realdir)
                          (begin
                            ((@@ (guix git)
                                 clone*) #$source realdir)
                            (format #t "Directory ~a was clonned into ~a. ~%" #$source realdir)))))))))

(define-public project-manager-service-type
  (service-type (name 'project-manager)
                (extensions
                 (list (service-extension home-activation-service-type activation)
                       (service-extension home-mcron-service-type mcron-fetcher)))
                (description
                 "Simple service to keep up all your git projects up to date with them git  remotes.
Example: (project-manager-conf
              (dir (string-append (getenv \"HOME\") \"/Projects\"))
              (projects
               (append
                (map channel->project (current-channels))
                (list
                 (project*
                  \"https://git.sr.ht/~sircmpwn/wlroots\"
                  \"wlroots\") ))))")))
