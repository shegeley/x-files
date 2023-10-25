(define-module (x-files services pman)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)
  #:use-module (x-files utils git)
  #:use-module (x-files utils project)

  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages ssh)

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
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix build utils)
  #:use-module (guix packages)

  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)

  #:export (<project-manager-conf>
            <project> project-manager-conf channel->project
            g-clone! g-fetch! project:dir project:source
            with-ssh-agent with-modules+exts fetcher-program-file
            project-manager:keys project-manager:dir
            project-manager:period project-manager:projects))

(define-record-type* <project> project make-project
  project?
  this-project
  (source project:source)
  (dir project:dir))

(define-public (project* s d)
  (project
   (source s)
   (dir d)))

(define-record-type* <project-manager-conf>
  project-manager-conf
  project-manager-conf!
  project-manager-conf~
  project-manager-conf?
  (keys project-manager:keys (default '()))
  (dir project-manager:dir)
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
           (ice-9 popen)
           (git)
           (guix build utils)
           (x-files utils git))
         #:select? modules-selector)
      body ...)))

(define (with-ssh-agent keys gexp*)
  "Evaluates @code{gexp*} with local (host-machine) ssh-agent running. Gets it @code{PID} and @code{AUTH_SOCK} and kill the process afterwards."
  (with-modules+exts
   #~(begin
       (use-modules
        (git)
        (guix build utils)
        (guix git)
        (ice-9 format)
        (ice-9 match)
        (ice-9 popen)
        (ice-9 rdelim)
        (ice-9 regex)
        (ice-9 textual-ports)
        (srfi srfi-9)
        (x-files utils git))

       (libgit2-init!)

       (let ((%ssh-agent-pid-regexp
              (make-regexp "SSH_AGENT_PID=(.*); export SSH_AGENT_PID;"))
             (%ssh-auth-sock-regexp
              (make-regexp "SSH_AUTH_SOCK=(.*); export SSH_AUTH_SOCK;"))
             (p (open-input-pipe "ssh-agent -s")))
         (let ((ssh-auth-sock-data (read-line p))
               (ssh-agent-pid-data (read-line p)))

           (when (or (eof-object? ssh-auth-sock-data)
                     (eof-object? ssh-agent-pid-data))
             (error "Could not start a SSH agent"))

           (close p)

           (let ((sockm (regexp-exec %ssh-auth-sock-regexp ssh-auth-sock-data))
                 (pidm  (regexp-exec %ssh-agent-pid-regexp ssh-agent-pid-data)))

             (unless (and sockm pidm)
               (error "Could not parse SSH agent response"
                      ssh-auth-sock-data
                      ssh-agent-pid-data))

             (let* [(sock (match:substring sockm 1))
                    (pid (match:substring pidm 1))
                    (pid* (string->number pid))
                    (ssh-agent-data
                     `((SSH_AUTH_SOCK ,sock)
                       (SSH_AGENT_PID ,pid)))]

               (map
                (lambda (p)
                  (let ((x (car p))
                        (y (cadr p)))
                    (setenv (symbol->string x) y)))
                ssh-agent-data)

               (map
                (lambda (k)
                  (invoke (string-append #$openssh "/bin/ssh-add") k))
                (quote #$keys))

               #$@gexp*

               ;; safe kill? (stolen from (guile-git tests sssd-ssshd))
               (when (false-if-exception pid*)
                 (kill pid* SIGTERM)))))))))

(define (g-fetch! config project)
  (match-record config <project-manager-conf>
    ((dir project-manager/dir))
    (match-record project <project>
      (source (dir project/dir))
      (let* ((realdir (string-append project-manager/dir "/" project/dir)))
        #~(fetch! #$realdir)))))

(define (fetcher-program-file config project)
  (program-file
   "project-manager-fetcher-script.scm"
   (with-ssh-agent (project-manager:keys config)
                   (list (g-fetch! config project)))))

(define (mcron-fetcher config)
  (match-record config <project-manager-conf>
    (period projects)
    (map (lambda (project)
           #~(job (lambda (t) (+ t #$period))
                  #$(fetcher-program-file config project)
                  "Project manager's fetcher mcron job"))
         projects)))

(define (channel->project channel)
  (match-record channel (@@ (guix channels) <channel>)
    (name url)
    (project* url (symbol->string name))))

(define (g-clone! config project)
  (match-record config <project-manager-conf>
    ((dir project-manager/dir))
    (match-record project <project>
      (source (dir project/dir))
      (let ((realdir (string-append project-manager/dir "/" project/dir)))
        #~(clone! #$source #$realdir)))))

(define (activation config)
  (match-record config <project-manager-conf>
    (projects)
    (with-ssh-agent
     (project-manager:keys config)
     (map
      (lambda (project)
        (g-clone! config project)) projects))))

(define-public project-manager-service-type
  (service-type
   (name 'project-manager)
   (compose concatenate)
   (extend (lambda (config projects*)
             (match-record config <project-manager-conf>
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
