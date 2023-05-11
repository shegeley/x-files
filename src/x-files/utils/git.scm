(define-module (x-files utils git)
  #:use-module (srfi srfi-1)

  #:use-module (guix build utils)

  #:use-module (git repository)
  #:use-module (git remote)
  #:use-module (git reference)
  #:use-module (git clone)
  #:use-module (git auth)
  #:use-module (git fetch)

  #:export (refs
            remotes
            with-repository
            with-remote
            with-repo-remotes
            fetch-remotes))

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
     (remote-fetch x
                   #:fetch-options fetch-options))
   #:remote-pred remote-pred))
