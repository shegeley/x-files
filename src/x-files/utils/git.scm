(define-module (x-files utils git)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)

  #:use-module (ice-9 peg)
  #:use-module (ice-9 match)

  #:use-module (guix build utils)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (git reference)
  #:use-module (git clone)
  #:use-module (git auth)
  #:use-module (git fetch)

  #:use-module (gnu services configuration)

  #:use-module (x-files utils records)

  #:export (refs remotes fetch-remotes clone! fetch!
                 remote remote:scheme remote:host remote:account remote:repository remote?
                 with-repository with-remotewith-repo-remotes
                 parse-git-scheme-uri parse-standart-uri))

(define (boolean-or-string? x)
  (or (boolean? x) (string x)))

(define-configuration/no-serialization remote-configuration
  (scheme boolean-or-string ""
           (default #f))
  (host string "")
  (account string "")
  (repository string ""))

(define-peg-pattern scheme all "git")

(define-peg-pattern at body "@")

(define-peg-pattern host all
  (* (and (not-followed-by colons) peg-any)))

(define-peg-pattern colons body ":")

(define-peg-pattern account all
  (* (and (not-followed-by slash) peg-any)))

(define-peg-pattern slash body "/")

(define-peg-pattern repository all
  (+ (and (not-followed-by ".git") peg-any)))

(define-peg-pattern git all
  (and scheme at host colons account slash repository))

(define (parse-git-scheme-uri git-uri)
  "Parses git-scheme scp/ssh-based uri (example: code@{git@github.com:account/repo.git}) and returns @code{remote} record instance"
  (match (peg:tree (match-pattern git git-uri))
    (('git ('scheme scheme)
           "@" ('host host) ":"
           ('account account) "/"
           ('repository repository))
     (remote-configuration
      (host host)
      (account account)
      (repository repository)))))

;; "ssh://github.com/sas/kek.git"
;; NOTE: parse path for (string->uri "ssh://github.com/sas/kek.git")
;; #<<uri> scheme: ssh userinfo: #f host: "github.com" port: #f path: "/sas/kek.git" query: #f fragment: #f>
(define (parse-standart-uri uri)
  "Parses standart RFC 3986 uri and returns @code{remote} record instance"

  (define-peg-pattern account all
    (* (and (not-followed-by slash) peg-any)))

  (define-peg-pattern path all (and slash account slash repository))

  (let* [(uri (string->uri uri))
         (path (match-pattern path (uri-path uri)))]
    (match (peg:tree path)
      (('path "/" ('account account)
              "/" ('repository repository))
       (remote-configuration
        (host host)
        (account account)
        (repository repository))))))

(define (with-remote remote thunk)
  (let* ((r (remote-connect remote))
         (t (thunk r))
         (_ (remote-disconnect r)))
    t))

(define (with-repository repo thunk)
  (let* ((r (repository-open repo))
         (t (thunk r))
         (_ (repository-close! r)))
    t))

(define (refs repo*)
  (with-repository
      repo*
      (lambda (r)
        (reference-fold cons '() r))))

(define (reference->remote repo* reference)
  (if (reference-remote? reference)
      (with-repository
          repo*
          (lambda (r)
            (remote-lookup
             r (third
                (string-split
                 (reference-name reference) #\/))))) #f))

(define (remotes repo*)
  (map
   (lambda (x)
     (reference->remote repo* x))
   (filter reference-remote?
           (refs repo*))))

(define* (with-repo-remotes
          repo* f
          #:key (remote-pred (const #t)))
  (map f (filter remote-pred (remotes repo*))))

(define* (fetch-remotes
          repo*
          #:key
          (remote-pred (const #t))
          fetch-options)
  (with-repo-remotes
   repo*
   (lambda (x)
     (remote-fetch x #:fetch-options fetch-options))
   #:remote-pred remote-pred))

(define (fetch! realdir)
  (with-exception-handler
      (lambda (exn)
        (format (current-error-port) "Git-repository ~a fetch failed. Exception: ~a ~%" realdir exn))
    (lambda ()
      (let ((opts (make-fetch-options)))
        (set-fetch-auth-with-ssh-agent! opts)
        (fetch-remotes realdir #:fetch-options opts)
        (format (current-output-port) "Git-repository ~a was successfully fetched. ~%" realdir)))
    #:unwind? #t))

(define (clone! source realdir)
  (with-exception-handler
      (lambda (exn)
        (format (current-error-port) "Git-repository ~a clone failed. Exception: ~a ~%" realdir exn))
    (lambda ()
      (cond ((and (directory-exists? realdir)
                  ;; NOTE: line below won't work as expected
                  ;; because some remotes in real directories are in ssh format (git@github.com:/user/repo) and some are in http (https://github.com/user/project)
                  ;; TODO: find a way to check equivalence
                  (false-if-exception (member source (map remote-url (remotes realdir)))))
             (format (current-error-port) "Directory ~s already exists. Skip cloning. ~%" realdir))
            (else (let ((opts (make-fetch-options)))
                    (set-fetch-auth-with-ssh-agent! opts)
                    (clone source realdir (make-clone-options #:fetch-options opts))
                    (format (current-output-port) "Directory ~a was clonned into ~a. ~%" source realdir)))))
    #:unwind? #t))
