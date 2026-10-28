(use-modules (guix gexp))
(use-modules ((ares suitbl) #:select (suite
                                      test
                                      is
                                      current-test-runner
                                      get-state
                                      make-suitbl))
             ((ares suitbl state) #:select (get-run-history))
             ((x-files build gitlab-runner) #:select (configure-runner!
                                                      read-config))
             ((x-files services gitlab-runner) #:select (gitlab-runner
                                                         gitlab-runner-configuration))
             ((toml) #:select (scm->toml
                               toml->scm))
             ((ice-9 textual-ports) #:select (get-string-all))
             ((srfi srfi-1) #:select (every)))

(define runner (make-suitbl #:config '((re-raise? . #t))))
(current-test-runner runner)
(define (fails? thunk)
  (catch #t
    (lambda ()
      (thunk)
      #f)
    (lambda _ #t)))

(suite "GitLab Runner configuration"
  (test "Configure the manager log destination" ()
    (is (equal? "/var/log/gitlab-runner.log"
                (assq-ref (gitlab-runner-configuration) 'log-files)))
    (is (equal? "/var/log/ci-runner.log"
                (assq-ref (gitlab-runner-configuration
                           #:log-files "/var/log/ci-runner.log")
                          'log-files)))
    (for-each (lambda (path)
                (is (fails? (lambda ()
                              (gitlab-runner-configuration #:log-files path)))))
              '(#f
                ""
                "runner.log"
                "/gnu/store/runner.log")))
  (test "Credential lifecycle, generation rollback and failed publication" ()
    (let* ((directory (mkdtemp "/tmp/gitlab-runner-unit.XXXXXX"))
           (token-file (string-append directory "/token"))
           (config-file (string-append directory "/config.toml"))
           (binary (cadr (command-line)))
           (settings '(("concurrent"     . 1)
                       ("check_interval" . 3)))
           (declarations (list (gitlab-runner #:name "test"
                                              #:url "http://127.0.0.1:1"
                                              #:token-file token-file))))
      (define (write-token value)
        (call-with-output-file token-file (lambda (port) (display value port))))
      (define (configure settings)
        (configure-runner! settings declarations directory
                           (lambda (file) (zero? (system* binary "lint" "--config" file)))))
      (define (credentials)
        (vector-ref (assoc-ref (read-config config-file) "runners") 0))
      (write-token "glrt-fixture-first\n")
      (configure settings)
      (is (equal? "glrt-fixture-first" (assoc-ref (credentials) "token")))
      (is (= #o600 (stat:perms (stat config-file))))
      (is (= #o600 (stat:perms (stat (string-append directory "/credential-seeds.scm")))))
      (let* ((current (read-config config-file))
             (entry (append '(("id" . 42))
                            (toml->scm "token_obtained_at = 2026-10-22T12:00:00Z\n")
                            (credentials))))
        (set-cdr! (assoc "token" entry) "glrt-rotated")
        (set-cdr! (assoc "runners" current) (vector entry))
        (call-with-output-file config-file (lambda (port) (scm->toml current port))))
      (configure '(("concurrent" . 2)))
      (is (= 2 (assoc-ref (read-config config-file) "concurrent")))
      (is (equal? "glrt-rotated" (assoc-ref (credentials) "token")))
      (is (= 42 (assoc-ref (credentials) "id")))
      (is (assoc-ref (credentials) "token_obtained_at"))
      (configure settings)
      (is (equal? "glrt-rotated" (assoc-ref (credentials) "token")))
      (write-token "glrt-replacement")
      (configure settings)
      (is (equal? "glrt-replacement" (assoc-ref (credentials) "token")))
      (is (not (assoc-ref (credentials) "id")))
      (let ((before (call-with-input-file config-file get-string-all)))
        (delete-file token-file)
        (is (fails? (lambda () (configure settings))))
        (is (equal? before (call-with-input-file config-file get-string-all)))
        (write-token "glrt-replacement")
        (is (fails? (lambda ()
                      (configure-runner! settings declarations directory (lambda _ #f)))))
        (is (equal? before (call-with-input-file config-file get-string-all)))
        (is (not (file-exists? (string-append directory "/config.candidate.toml")))))
      (let ((current (read-config config-file)))
        (set-cdr! (assoc "token" (vector-ref (assoc-ref current "runners") 0)) "glrt-retired-rotated")
        (call-with-output-file config-file (lambda (port) (scm->toml current port))))
      (configure-runner! settings '() directory (lambda _ #t))
      (is (not (assoc-ref (read-config config-file) "runners")))
      (configure settings)
      (is (equal? "glrt-retired-rotated" (assoc-ref (credentials) "token")))))
  (test "Adding and reordering projects preserves each runner's credentials" ()
    (let* ((directory (mkdtemp "/tmp/gitlab-runner-projects.XXXXXX"))
           (config-file (string-append directory "/config.toml"))
           (first-token (string-append directory "/first.token"))
           (second-token (string-append directory "/second.token"))
           (binary (cadr (command-line)))
           (settings '(("concurrent" . 1)))
           (first (gitlab-runner #:name "first"
                                 #:token-file first-token))
           (second (gitlab-runner #:name "second"
                                  #:token-file second-token)))
      (define (configure declarations)
        (configure-runner! settings declarations directory
                           (lambda (file) (zero? (system* binary "lint" "--config" file)))))
      (define (entries)
        (vector->list (assoc-ref (read-config config-file) "runners")))
      (call-with-output-file first-token (lambda (port) (display "glrt-fixture-first" port)))
      (call-with-output-file second-token (lambda (port) (display "glrt-fixture-second" port)))
      (configure (list first))
      (let* ((current (read-config config-file))
             (entry (vector-ref (assoc-ref current "runners") 0)))
        (set-cdr! (assoc "token" entry) "glrt-fixture-rotated")
        (set-cdr! (assoc "runners" current) (vector (cons '("id" . 42) entry)))
        (call-with-output-file config-file (lambda (port) (scm->toml current port))))
      (configure (list first second))
      (is (equal? '("first"
                    "second") (map (lambda (entry) (assoc-ref entry "name")) (entries))))
      (is (equal? '("glrt-fixture-rotated"
                    "glrt-fixture-second")
                  (map (lambda (entry) (assoc-ref entry "token")) (entries))))
      (is (= 42 (assoc-ref (car (entries)) "id")))
      (configure (list second first))
      (is (equal? '("glrt-fixture-second"
                    "glrt-fixture-rotated")
                  (map (lambda (entry) (assoc-ref entry "token")) (entries))))
      (is (= 42 (assoc-ref (cadr (entries)) "id")))
      (let ((before (call-with-input-file config-file get-string-all)))
        (delete-file second-token)
        (is (fails? (lambda () (configure (list first second)))))
        (is (equal? before (call-with-input-file config-file get-string-all))))
      (configure (list first))
      (is (equal? '("glrt-fixture-rotated")
                  (map (lambda (entry) (assoc-ref entry "token")) (entries))))))
  (test "Reject store secrets, duplicate names and inline credentials" ()
    (is (fails? (lambda () (gitlab-runner #:name "bad"
                                          #:token-file "/gnu/store/secret"))))
    (is (fails? (lambda ()
                  (gitlab-runner #:name "bad"
                                 #:token-file "/run/secret"
                                 #:settings '(("token" . "inline"))))))
    (is (fails? (lambda ()
                  (let ((entry (gitlab-runner #:name "duplicate"
                                              #:token-file "/run/secret")))
                    (gitlab-runner-configuration #:runners (list entry entry))))))))

(let ((history (get-run-history (get-state runner))))
  (exit (if (and (pair? history)
                 (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome))) history))
            0
            1)))
