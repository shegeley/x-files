(define-module (x-files tests fixtures gitlab-runner-coordinator)
  #:use-module (json)
  #:use-module ((ice-9 match) #:select (match))
  #:use-module ((rnrs bytevectors) #:select (utf8->string
                                             bytevector-length))
  #:use-module ((rnrs io ports) #:select (put-bytevector
                                          call-with-port))
  #:use-module ((web http) #:select (declare-header!))
  #:use-module ((web request) #:select (request-method
                                        request-uri
                                        request-headers))
  #:use-module ((web response) #:select (build-response))
  #:use-module ((web server) #:select (run-server))
  #:use-module ((web uri) #:select (uri-path))
  #:export (run-coordinator))

(define (job id script)
  "Return a shell job with @var{id} and @var{script} for Runner 19.5.1
and its @code{jobs/request} protocol."
  `(("id"              . ,id)
    ("token"           . "fixture-job-token")
    ("allow_git_fetch" . #f)
    ("job_info"        . (("name"       . "lifecycle")
                          ("stage"      . "test")
                          ("project_id" . 1)))
    ("git_info"        . (("repo_url" . "http://127.0.0.1:8091/fixture/project.git")
                          ("ref"      . "main")
                          ("sha"      . ,(make-string 40 #\a))
                          ("ref_type" . "branch")))
    ("runner_info"     . (("timeout" . 90)))
    ("variables"       . #((("key"   . "GIT_STRATEGY")
                            ("value" . "none"))
                           (("key"   . "CI_JOB_ID")
                            ("value" . ,(number->string id)))))
    ("steps"           . #((("name"          . "script")
                            ("when"          . "on_success")
                            ("allow_failure" . #f)
                            ("script"        . #(,(string-append "bash " script))))
                           (("name"          . "after_script")
                            ("when"          . "always")
                            ("allow_failure" . #f)
                            ("script"        . #(,(string-append "bash " script " after"))))))))

(define (run-coordinator directory script)
  "Serve dummy jobs running @var{script} on VM loopback.  Control and
evidence files live in @var{directory}.  Write an integer to @file{offer} to
queue a job.  Result JSON and trace bytes are kept per job.  No registration,
repository, registry or real GitLab is emulated."
  (define (path name)
    (string-append directory "/" name))
  (define (save name value)
    (let ((temporary (path (string-append name ".tmp"))))
      (call-with-output-file temporary (lambda (port) (scm->json value port)))
      (rename-file temporary (path name))))
  (define* (reply status #:optional
                  data
                  (headers '()))
    (values (build-response #:code status
                            #:headers (append '((content-type . (application/json)))
                                              headers))
            (if data
                (scm->json-string data)
                "")))
  (define (handle request body)
    (let ((route (string-split (uri-path (request-uri request)) #\/))
          (method (request-method request)))
      (match (cons method route)
        (('POST "" "api" "v4" "jobs" "request")
         (let ((data (json-string->scm (utf8->string body))))
           (cond
            ((not (equal? "glrt-offline-fixture" (assoc-ref data "token")))
             (reply 403))
            ((file-exists? (path "offer"))
             (let* ((id (call-with-input-file (path "offer") read))
                    (name (string-append "assigned-" (number->string id))))
               (delete-file (path "offer"))
               (save name data)
               (reply 201 (job id script))))
            (else
             (reply 204)))))
        (('PATCH "" "api" "v4" "jobs" id "trace")
         (let* ((file (path (string-append "trace-" id)))
                (offset (if (file-exists? file)
                            (stat:size (stat file))
                            0))
                (range (assoc-ref (request-headers request) 'content-range))
                (expected (string-append (number->string offset) "-"
                                         (number->string (- (+ offset (bytevector-length body)) 1)))))
           (if (and (equal? "fixture-job-token" (assoc-ref (request-headers request) 'job-token))
                    (equal? expected range))
               (begin
                 (call-with-port (open-file file "ab") (lambda (port) (put-bytevector port body)))
                 (reply 202 #f
                        `((range . ,(string-append "0-" (number->string (stat:size (stat file))))))))
               (begin
                 (save "protocol-error" "invalid trace token or offset")
                 (reply 400)))))
        (('PUT "" "api" "v4" "jobs" id)
         (let ((data (json-string->scm (utf8->string body))))
           (if (equal? "fixture-job-token" (assoc-ref data "token"))
               (begin
                 (save (string-append "result-" id) data)
                 (reply 200))
               (reply 403))))
        (_
         (save "protocol-error" (uri-path (request-uri request)))
         (reply 404)))))
  ;; Runner trace ranges omit the HTTP bytes unit.  Override only in this fixture.
  (for-each (lambda (name)
              (declare-header! name (lambda (value) value) string? display))
            '("Content-Range"
              "Range"))
  (unless (file-exists? directory)
    (mkdir directory #o755))
  (run-server handle 'http '(#:host "127.0.0.1"
                             #:port 8091)))
