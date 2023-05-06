(define-module (x-files services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)
  #:use-module (x-files utils git)
  #:use-module (x-files utils project)

  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)

  #:use-module (gnu services configuration)
  #:use-module (gnu home services mcron)

  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (git auth)
  #:use-module (git fetch)
  #:use-module (git clone)
  #:use-module (git bindings)

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
            channel->project
            clone!
            fetch!
            g-clone!
            g-fetch!
            project:dir
            project:source))

(define-record-type* <project> project make-project
  project?
  this-project
  (source project:source)
  (dir project:dir)
  (auth-method project:auth-method
               (default (%make-auth-ssh-agent))))

(define-public (project* s d)
  (project
   (source s)
   (dir d)))

(define-record-type* <project-manager-conf>
  project-manager-conf
  project-manager-conf!
  project-manager-conf~
  project-manager-conf?
  (dir project-manager:dir)
  (auth-method project-manager:auth-method
               (default (%make-auth-ssh-agent)))
  (period project-manager:period (default 300))
  ;; project can be a project* a channel or a package
  (projects project-manager:projects))

(define (modules-selector name)
  (or ((@@ (guix modules) guix-module-name?) name)
      (match name
        (('srfi ..) #t)
        (('x-files 'utils ..) #t)
        (('git ..) #t)
        (else #f))))

(define-syntax-rule (with-modules+exts body ...)
  (with-extensions
      (list guile-git
            guile-bytestructures
            guile-gcrypt)
    (with-imported-modules
        (source-module-closure
         '((ice-9 format)
           (guix build utils)
           (x-files utils git))
         (append %load-path
                 (list (git-project-dir)))
         #:select? modules-selector)
      body ...)))

(define* (template
          config body
          ;; body := procedure of 2 arguments <project-manager-conf> <project>
          #:key (wrapper identity))
  (match-record
      config <project-manager-conf>
    (projects)
    (wrapper
     (with-modules+exts
      #~(begin
          (use-modules
           (git bindings) (ice-9 format)
           (git fetch) (git clone) (git auth)
           (x-files utils git)
           (guix build utils))

          #$@(map
              (lambda (project)
                (body config project)) projects))))))

(define* (fetch! realdir
                 #:optional auth-method)
  #~(begin
      (libgit2-init!)
      ;; TODO: add a way to fetch git with credentials
      (false-if-exception
       (begin
         (fetch-remotes
          #$realdir
          #:fetch-options (make-fetch-options (%make-auth-ssh-agent)))
         (format #t "Git-repository ~a was successfully fetched. ~%" #$realdir)))))

(define (g-fetch! config
                  project)
  (match-record
      config <project-manager-conf>
    ((dir project-manager/dir)
     (auth-method project-manager/auth-method))
    (match-record
        project <project>
      (source
       (dir project/dir)
       (auth-method project/auth-method))
      (let* ((auth-method (or project/auth-method
                              project-manager/auth-method))
             (realdir (string-append project-manager/dir "/" project/dir)))
        (fetch! realdir auth-method)))))

(define (fetcher-program-file config)
  ;; TODO: create template from both fetcher and activatoion.
  ;; reduce code duplication
  (template config g-fetch!
            #:wrapper
            (lambda (x)
              (program-file
               "project-manager-fetcher-script.scm" x))))

(define (mcron-fetcher config)
  (match-record
      config <project-manager-conf>
    (period)
    (list #~(job (lambda (t) (+ t #$period))
                 #$(fetcher-program-file config)
                 "Project manager's fetcher daemon"))))

(define (channel->project channel)
  (match-record channel (@@ (guix channels) <channel>)
    (name url)
    (project* url (symbol->string name))))

(define* (clone! source realdir
                 #:optional
                 auth-method)
  #~(begin
      (display "1 2 3 \n")
      (if (directory-exists? #$realdir)
          (format #t "Directory ~s already exists. Skip cloning. ~%" #$realdir)
          (begin
            ((@@ (guix git)
                 clone) #$source #$realdir
                 (make-clone-options
                  #:fetch-options
                  (make-fetch-options
                   (%make-auth-ssh-agent))))
            (format #t "Directory ~a was clonned into ~a. ~%" #$source #$realdir)))))

(define (g-clone! config
                  project)
  (match-record
      config <project-manager-conf>
    ((auth-method project-manager/auth-method)
     (dir project-manager/dir))
    (match-record
        project <project>
      (source
       (dir project/dir)
       (auth-method project/auth-method))
      (let ((realdir (string-append project-manager/dir "/" project/dir))
            (auth-method (or project-manager/auth-method
                             project/auth-method)))
        (clone! source realdir)))))

(define (activation config)
  (template
   config
   (lambda (config project)
     (g-clone! config project))))

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
