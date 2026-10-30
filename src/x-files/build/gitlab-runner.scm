(define-module (x-files build gitlab-runner)
  #:use-module ((x-files utils alist) #:select (match-alist))
  ;; Supplied by runner-program's with-extensions at runtime.
  #:autoload (toml) (toml->scm
                     scm->toml)
  #:use-module ((gcrypt hash) #:select (sha256))
  #:use-module ((gcrypt base16) #:select (bytevector->base16-string))
  #:use-module ((rnrs bytevectors) #:select (string->utf8))
  #:use-module ((ice-9 textual-ports) #:select (get-string-all))
  #:use-module ((srfi srfi-1) #:select (find
                                        alist-delete
                                        filter))
  #:use-module ((srfi srfi-13) #:select (string-trim-both
                                         string-prefix?
                                         string-any))
  #:export (configure-runner!
            runner-with-job-path
            read-config))

(define (runner-with-job-path runner path)
  "Give shell jobs the manager's @var{path} after login profile loading.
Preserve an explicitly declared PATH and all other runner settings."
  (match-alist runner (settings)
    (let ((environment (vector->list (or (assoc-ref settings "environment") #()))))
      (if (and (equal? "shell" (assoc-ref settings "executor"))
               (not (find (lambda (entry) (string-prefix? "PATH=" entry)) environment)))
          (acons 'settings
                 (acons "environment"
                        (list->vector (cons (string-append "PATH=" path) environment))
                        (alist-delete "environment" settings))
                 (alist-delete 'settings runner))
          runner))))

(define (read-config path)
  "Read the Runner TOML file at @var{path} without printing its credentials."
  (toml->scm (call-with-input-file path get-string-all)))

(define (atomic-write path writer)
  (let* ((temporary (string-append path ".XXXXXX"))
         (port (mkstemp! temporary)))
    (dynamic-wind
      (lambda () #t)
      (lambda ()
        (chmod temporary #o600)
        (writer port)
        (force-output port)
        (fsync port)
        (close-port port)
        (rename-file temporary path))
      (lambda ()
        (unless (port-closed? port)
          (close-port port))
        (when (file-exists? temporary)
          (delete-file temporary))))))

(define (read-runner-state directory)
  "Read credential history and seed metadata from @var{directory} as an
alist.  Active runners take precedence over retired entries with the same name."
  (let* ((config-file (string-append directory "/config.toml"))
         (seed-file (string-append directory "/credential-seeds.scm"))
         (history-file (string-append directory "/retired-runners.toml"))
         (previous (if (file-exists? config-file)
                       (read-config config-file)
                       '()))
         (active (vector->list (or (assoc-ref previous "runners") #())))
         (retired (if (file-exists? history-file)
                      (vector->list
                       (or (assoc-ref (read-config history-file) "runners") #()))
                      '()))
         (history
          (append active
                  (filter (lambda (entry)
                            (not (find (lambda (old)
                                         (equal? (assoc-ref entry "name")
                                                 (assoc-ref old "name")))
                                       active)))
                          retired)))
         (seeds (if (file-exists? seed-file)
                    (call-with-input-file seed-file read)
                    '())))
    `((history . ,history)
      (seeds . ,seeds))))

(define (reconcile-runner runner history seeds)
  "Return the TOML entry and seed identity for @var{runner} as an alist.
Reuse credentials from @var{history} when the URL and @var{seeds} still match."
  (match-alist runner (name url token-file settings)
    (let* ((token (string-trim-both
                   (call-with-input-file token-file get-string-all)))
           (fingerprint (bytevector->base16-string (sha256 (string->utf8 token))))
           (identity (list name url fingerprint))
           (old (find (lambda (entry) (equal? name (assoc-ref entry "name")))
                      history))
           (credentials '("token"
                          "id"
                          "token_obtained_at"
                          "token_expires_at"))
           (reuse-credentials? (and old
                                    (equal? url (assoc-ref old "url"))
                                    (equal? identity (assoc-ref seeds name)))))
      (unless (and (string-prefix? "glrt-" token)
                   (not (string-any char-whitespace? token)))
        (error "Expected a runner authentication token in token-file"))
      (when (and old (not (assoc name seeds)))
        (error "Existing runner has no seed metadata; explicit migration is required"))
      (let ((entry (append
                    `(("name" . ,name)
                      ("url"  . ,url))
                    (if reuse-credentials?
                        (filter (lambda (pair) (member (car pair) credentials))
                                old)
                        `(("token" . ,token)))
                    settings)))
        `((entry . ,entry)
          (seed-identity . ,identity))))))

(define (reconcile-runners runners history seeds)
  "Return ordered TOML entries for @var{runners} and updated seed metadata
using @var{history} and @var{seeds}.  Retain removed runners' seeds for rollback."
  (let* ((new-seeds seeds)
         (entries
          (map (lambda (runner)
                 (match-alist runner (name)
                   (let ((reconciled (reconcile-runner runner history seeds)))
                     (match-alist reconciled (entry seed-identity)
                       (set! new-seeds
                             (acons name
                                    seed-identity
                                    (filter (lambda (seed)
                                              (not (equal? name (car seed))))
                                            new-seeds)))
                       entry))))
               runners)))
    `((entries . ,entries)
      (seeds . ,new-seeds))))

(define (publish-runner-configuration! settings reconciled history directory validate)
  "Write @var{settings} and @var{reconciled} to @var{directory} after
@var{validate} accepts the candidate.  Save @var{history} before replacing
@file{config.toml}, then publish seed metadata."
  (match-alist reconciled (entries seeds)
    (let ((config-file (string-append directory "/config.toml"))
          (seed-file (string-append directory "/credential-seeds.scm"))
          (history-file (string-append directory "/retired-runners.toml"))
          (candidate (string-append directory "/config.candidate.toml")))
      (dynamic-wind
        (lambda () #t)
        (lambda ()
          (atomic-write candidate
                        (lambda (port)
                          (scm->toml (append settings
                                             `(("runners" . ,(list->vector entries))))
                                     port)))
          (unless (validate candidate)
            (error "GitLab Runner rejected the generated configuration"))
          ;; Retain rotated credentials before removing a runner from config, so
          ;; rolling back its declaration can restore it without re-registration.
          (atomic-write history-file
                        (lambda (port)
                          (scm->toml `(("runners" . ,(list->vector history))) port)))
          (rename-file candidate config-file)
          ;; Publish after config: an interrupted seed change is safely repeated.
          (atomic-write seed-file (lambda (port)
                                    (write seeds port)
                                    (newline port))))
        (lambda ()
          (when (file-exists? candidate)
            (delete-file candidate)))))))

(define (configure-runner! settings runners directory validate)
  "Reconcile public @var{settings} and @var{runners} with mutable
credentials in @var{directory}.  The procedure @var{validate} checks a
temporary TOML file before publication as @file{config.toml}.  A changed seed
file replaces credentials; an unchanged seed preserves GitLab's rotated token."
  (let ((previous-state (read-runner-state directory)))
    (match-alist previous-state (history seeds)
      (let ((reconciled (reconcile-runners runners history seeds)))
        (publish-runner-configuration! settings reconciled history directory validate)))))
