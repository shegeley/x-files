(define-module (x-files utils ssh-agent)
  #:use-module (gnu)
  #:use-module (gnu system)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)
  #:use-module (x-files utils git)
  #:use-module (x-files utils project)

  #:use-module (gnu packages ssh)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 exceptions)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export-syntax (with-tmp-ssh-agent))

(define-syntax-rule (with-tmp-ssh-agent keys body ...)
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

        ;; DEBUG LINES
        ;; (let ((p (open-input-pipe (string-append #$(file-append openssh "/bin/ssh-agent") " " "-s"))))
        ;;   (format #t "From /bin/ssh-agent: ~a ~%" (get-string-all p)))

        ;; (let ((p (open-input-pipe
        ;;           (string-append #$(file-append openssh "/bin/ssh-add") " " "-l"))))
        ;;   (format #t "From /bin/ssh-add: ~a ~%" (get-string-all p)))

        ;; (let ((v "SSH_AUTH_SOCK"))
        ;;   (format #t "Var: ~a, env: ~a ~%" v (getenv v)))

        ;; (let ((v "SSH_AGENT_PID"))
        ;;   (format #t "Var: ~a, env: ~a ~%" v (getenv v)))

        (let ((ssh-agent-data
               `((SSH_AUTH_SOCK . ,(match:substring sockm 1))
                 (SSH_AGENT_PID . ,(match:substring pidm 1)))))

          (map
           (match-lambda
             ((x . y)
              (setenv (symbol->keyword x) y))) ssh-agent-data)

          (map
           (lambda (k)
             (invoke #$(file-append openssh "/bin/ssh-add") k)) #$keys)

          body ...

          ;; safe kill (stolen from (guile-git tests sssd-ssshd))
          (when (false-if-exception (string->number pidm))
            (kill pid SIGTERM)
            (catch #t
              (lambda ()
                (waitpid pid))
              (const #t))))))))
