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
               (default 'agent)))

(define (auth-method! x)
  "This is a draft to support other auth-methods later"
  (match x
    ('agent
     ;; NOTE: WON'T WORK, USE set-fetch-auth-with-…!
     #~(%make-auth-ssh-agent))
    ;; NOTE: the only option to make creds work properly is to call set-fetch-auth-with-…! explicitly, (%make-auth-ssh-agent) won't be interned to the store properly :(
    (_
     (throw 'pman:unsupported-auth-method x))))

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
               (default 'agent))
  (period project-manager:period (default (* 60 10)))
  ;; project can be a project* a channel or a package
  (projects project-manager:projects))

(define (modules-selector name)
  (or ((@@ (guix modules) guix-module-name?) name)
      (match name
        (('srfi ..) #t)
        (('x-files 'utils ..) #t)
        (('git) #t)
        (else #f))))

(define-syntax-rule (with-modules+exts body ...)
  (with-extensions
   (list guile-git
         guile-bytestructures
         guile-gcrypt
         guile-zlib)
   (with-imported-modules
    (source-module-closure
     '((ice-9 format)
       (git)
       (guix build utils)
       (x-files utils git))
     #:select? modules-selector)
    body ...)))

(define-syntax template
  (syntax-rules ()
    ((_ config body wrapper)
     (match-record
      config <project-manager-conf>
      (projects)
      (wrapper
       (with-modules+exts
        #~(begin
            (use-modules
             (srfi srfi-9)
             (git)
             (ice-9 format)
             (x-files utils git)
             (guix git)
             (guix build utils))

            (libgit2-init!)

            (let ((v "SSH_AUTH_SOCK"))
              (format #t "Var: ~a, env: ~a ~%" v (getenv v)))

            (let ((v "SSH_AGENT_PID"))
              (format #t "Var: ~a, env: ~a ~%" v (getenv v)))

            #$@(map
                (lambda (project)
                  (body config project)) projects))))))
    ((_ config body)
     (template config body identity))))

(define* (fetch! realdir
                 #:optional auth-method)
  #~(with-exception-handler
        (lambda (exn)
          (format #t "Git-repository ~a fetch failed. Exception: ~a ~%" #$realdir exn))
      (lambda ()
        (let ((opts (make-fetch-options)))
          ;; NOTE: the only option to make it work properly is to call set-fetch-auth-with-…! explicitly, (%make-auth-ssh-agent) won't be interned to the store properly :(
          (set-fetch-auth-with-ssh-agent! opts)
          (fetch-remotes
           #$realdir
           #:fetch-options opts)
          (format #t "Git-repository ~a was successfully fetched. ~%" #$realdir)))
      #:unwind? #t))

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
            (lambda (x)
              (program-file
               "project-manager-fetcher-script.scm" x))))

(define (mcron-fetcher config)
  (match-record
   config <project-manager-conf>
   (period)
   (list (with-modules+exts
          #~(job (lambda (t) (+ t #$period))
                 #$(fetcher-program-file config)
                 "Project manager's fetcher daemon")))))

(define (channel->project channel)
  (match-record channel (@@ (guix channels) <channel>)
                (name url)
                (project* url (symbol->string name))))

(define* (clone! source realdir
                 #:optional
                 auth-method)
  #~(if (directory-exists? #$realdir)
        (format #t "Directory ~s already exists. Skip cloning. ~%" #$realdir)
        (let ((opts (make-fetch-options)))
          ;; NOTE: the only option to make it work properly is to call set-fetch-auth-with-…! explicitly, (%make-auth-ssh-agent) won't be interned to the store properly :(
          (set-fetch-auth-with-ssh-agent! opts)
          (clone #$source #$realdir
                 (make-clone-options
                  #:fetch-options
                  opts))
          (format #t "Directory ~a was clonned into ~a. ~%" #$source #$realdir))))

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
      (clone! source realdir auth-method)))))

(define (activation config)
  (template config g-clone!))

(define-public project-manager-service-type
  (service-type
   (name 'project-manager)
   (compose concatenate)
   (extend (lambda (config projects*)
             (match-record
              config <project-manager-conf>
              (projects)
              (project-manager-conf
               (inherit config)
               (projects (append projects projects*))))))
   (extensions
    (list (service-extension home-activation-service-type activation)
          (service-extension home-mcron-service-type mcron-fetcher)))
   (description
    "Simple service to keep up all your git projects up to date with their git remotes.
@example
(project-manager-conf
    (dir (string-append (getenv \"HOME\") \"/Projects\"))
    (projects
        (append
        (map channel->project (current-channels))
        (list
            (project*
            \"https://git.sr.ht/~sircmpwn/wlroots\"
            \"wlroots\") ))))
@end example")))
