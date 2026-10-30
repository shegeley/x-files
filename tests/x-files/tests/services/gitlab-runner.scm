(define-module (x-files tests services gitlab-runner)
  #:use-module (guix gexp)
  #:use-module ((gnu tests) #:select (simple-operating-system
                                      marionette-operating-system
                                      system-test))
  #:use-module ((gnu services) #:select (service
                                         simple-service
                                         extra-special-file))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service
                                                  shepherd-service-file
                                                  shepherd-root-service-type))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((gnu packages admin) #:select (shepherd))
  #:use-module ((gnu packages base) #:select (hello))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((gnu packages package-management) #:select (guix))
  #:use-module ((gnu packages guile-xyz) #:select (guile-toml))
  #:use-module ((gnu packages guile) #:select (guile-json-4))
  #:use-module ((guix scripts system reconfigure) #:select (switch-system-program
                                                            upgrade-services-program))
  #:use-module ((x-files packages gitlab-runner) #:select (gitlab-runner-bin))
  #:use-module ((x-files services gitlab-runner) #:select (gitlab-runner
                                                           gitlab-runner-configuration
                                                           gitlab-runner-service-type))
  #:export (%test-gitlab-runner))

(define (config concurrent)
  (gitlab-runner-configuration
   #:stop-timeout 90
   #:extra-packages (list hello)
   #:log-files (if (= concurrent 1)
                   "/var/log/gitlab-runner.log"
                   "/var/log/gitlab-runner-custom.log")
   #:settings `(("concurrent"     . ,concurrent)
                ("check_interval" . 1))
   #:runners (list (gitlab-runner #:name "offline-fixture"
                                  #:url "http://127.0.0.1:8091"
                                  #:token-file "/etc/gitlab-runner-token"
                                  #:settings '(("executor" . "shell")
                                               ("shell" . "bash")
                                               ("environment" . #("RUNNER_FIXTURE=present")))))))

(define coordinator
  (with-extensions (list guile-json-4)
    (with-imported-modules '((x-files tests fixtures gitlab-runner-coordinator))
      (program-file "gitlab-runner-coordinator"
        #~(begin
            (use-modules ((x-files tests fixtures gitlab-runner-coordinator) #:select (run-coordinator)))
            (run-coordinator "/tmp/gitlab-runner-fixture"
                             #$(local-file (canonicalize-path (search-path %load-path "fixtures/gitlab-runner-job.sh")))))))))

(define (test-os concurrent)
  (marionette-operating-system
   (simple-operating-system
    (service dhcpcd-service-type)
    (simple-service 'runner-coordinator shepherd-root-service-type
                    (list (shepherd-service
                            (provision '(runner-coordinator))
                            (requirement '(user-processes))
                            (start #~(make-forkexec-constructor
                                      (list #$coordinator)
                                      #:log-file "/var/log/runner-coordinator.log"))
                            (stop #~(make-kill-destructor)))))
    ;; Public dummy credential: never connects to a real GitLab instance.
    (extra-special-file "/etc/gitlab-runner-token" (plain-file "runner-fixture-token" "glrt-offline-fixture"))
    (service gitlab-runner-service-type (config concurrent)))
   #:imported-modules '((gnu services herd) (guix combinators))))

(define (manager concurrent)
  (car ((@@ (x-files services gitlab-runner) runner-shepherd-services) (config concurrent))))

(define (replace-manager manager)
  (upgrade-services-program (list (shepherd-service-file manager)) '() '() '(gitlab-runner)))

(define (run-test)
  (define old-os (test-os 1))
  (define new-os (test-os 2))
  (define vm (virtual-machine (operating-system old-os)
                              (memory-size 2048)))
  (define activation (program-file "activate-runner"
                       ((@@ (x-files services gitlab-runner) runner-activation) (config 1))))
  (define failing-manager (shepherd-service (inherit (manager 1))
                                            (respawn? #f)))
  (gexp->derivation
      "gitlab-runner-lifecycle"
    (with-extensions (list guile-toml guile-json-4)
      (with-imported-modules '((gnu build marionette))
        #~(begin
            (use-modules ((gnu build marionette) #:select (make-marionette
                                                           marionette-eval
                                                           system-test-runner))
                         ((srfi srfi-64) #:select (test-runner-current
                                                   test-begin
                                                   test-assert
                                                   test-equal
                                                   test-end))
                         ((toml) #:select (toml->scm))
                         (json))
            (define marionette (make-marionette (list #$vm)))
            (define (guest form)
              (marionette-eval form marionette))
            (define (action verb)
              (guest `(zero? (system* #$(file-append shepherd "/bin/herd") ,verb "gitlab-runner"))))
            (define (load-script file)
              (guest `(begin
                        (primitive-load ,file)
                        #t)))
            (define (eventually predicate)
              (let loop ((remaining 40))
                (cond
                 ((predicate)
                  #t)
                 ((zero? remaining)
                  #f)
                 (else
                  (sleep 1)
                  (loop (- remaining 1))))))
            (define (pids)
              (guest '(begin
                        (use-modules ((ice-9 ftw) #:select (scandir))
                                     ((srfi srfi-1) #:select (filter-map)))
                        (filter-map
                         (lambda (name)
                           (catch 'system-error
                             (lambda ()
                               (and (equal? (readlink (string-append "/proc/" name "/exe"))
                                            #$(file-append gitlab-runner-bin "/bin/gitlab-runner"))
                                    (string->number name)))
                             (lambda _ #f)))
                         (scandir "/proc" (lambda (name) (string->number name)))))))
            (define (running?)
              (let ((ids (pids)))
                (and (= 1 (length ids))
                     (guest `(begin
                               (use-modules ((ice-9 textual-ports) #:select (get-string-all)))
                               (and (file-exists? "/var/lib/gitlab-runner/.runner_system_id")
                                    (> (stat:size (stat "/var/lib/gitlab-runner/.runner_system_id")) 0)
                                    (string-contains
                                     (call-with-input-file
                                         ,(string-append "/proc/" (number->string (car ids)) "/cmdline")
                                       get-string-all)
                                     (string #\nul #\r #\u #\n #\nul))))))))
            (define (stopped?)
              (null? (pids)))
            (define (contents file)
              (guest `(begin
                        (use-modules ((ice-9 textual-ports) #:select (get-string-all)))
                        (call-with-input-file ,file get-string-all))))
            (define (concurrent? n)
              (= n (assoc-ref (toml->scm (contents "/var/lib/gitlab-runner/config.toml")) "concurrent")))
            (define (exists? file)
              (guest `(file-exists? ,file)))
            (define (write-file file value)
              (guest `(let ((temporary (string-append ,file ".tmp")))
                        (call-with-output-file temporary (lambda (port) (display ,value port)))
                        (rename-file temporary ,file)
                        #t)))
            (define (job-file id suffix)
              (string-append "/var/lib/gitlab-runner/job-" (number->string id) "." suffix))
            (define (fixture-file name id)
              (string-append "/tmp/gitlab-runner-fixture/" name "-" (number->string id)))
            (define (finished? id state)
              (let ((file (fixture-file "result" id)))
                (and (exists? file)
                     (equal? state (assoc-ref (json-string->scm (contents file)) "state")))))
            (test-runner-current (system-test-runner #$output))
            (test-begin "gitlab-runner-lifecycle")
            (define ready? (eventually running?))
            (test-assert "real manager starts" ready?)
            (unless ready?
              (display (contents "/var/log/gitlab-runner.log") (current-error-port))
              (test-end))
            (test-assert "Shepherd status" (action "status"))
            (test-assert "unprivileged process and private configuration"
              (guest `(let* ((account (getpwnam "gitlab-runner"))
                             (state (stat "/var/lib/gitlab-runner/config.toml")))
                        (and (> (passwd:uid account) 0)
                             (= (stat:uid (stat ,(string-append "/proc/" (number->string (car (pids))))))
                                (passwd:uid account))
                             (= (stat:uid state) (passwd:uid account))
                             (= #o600 (stat:perms state))))))
            (define identity (contents "/var/lib/gitlab-runner/.runner_system_id"))
            (test-assert "initial public settings" (concurrent? 1))
            (test-assert "stop leaves no manager" (and (action "stop") (eventually stopped?)))
            (test-assert "repeated stop is harmless" (begin
                                                       (action "stop")
                                                       (stopped?)))
            (test-assert "second start" (and (action "start") (eventually running?)))
            (test-equal "runner identity survives restart" identity
                        (contents "/var/lib/gitlab-runner/.runner_system_id"))
            (test-assert "activation is idempotent" (and (load-script #$activation) (load-script #$activation)))
            (test-assert "generation switch leaves current manager until restart"
              (let ((before (pids)))
                (and (load-script #$(switch-system-program new-os))
                     (load-script #$(replace-manager (manager 2)))
                     (equal? before (pids)))))
            (test-assert "new generation settings take effect on restart"
              (and (action "stop")
                   (eventually stopped?)
                   (action "start")
                   (eventually running?)
                   (concurrent? 2)))
            (test-assert "new generation writes to the configured log path"
              (eventually
               (lambda ()
                 (and (exists? "/var/log/gitlab-runner-custom.log")
                      (string-contains
                       (contents "/var/log/gitlab-runner-custom.log")
                       "Starting multi-runner")))))
            (define default-log-size
              (string-length (contents "/var/log/gitlab-runner.log")))
            (test-assert "rollback restores previous settings"
              (and (load-script #$(switch-system-program old-os))
                   (load-script #$(replace-manager (manager 1)))
                   (action "stop")
                   (eventually stopped?)
                   (action "start")
                   (eventually running?)
                   (concurrent? 1)))
            (test-assert "rollback resumes the previous log path"
              (eventually
               (lambda ()
                 (> (string-length (contents "/var/log/gitlab-runner.log"))
                    default-log-size))))
            (test-equal "rollback retains runner identity" identity
                        (contents "/var/lib/gitlab-runner/.runner_system_id"))
            (test-assert "stop before missing-secret failure" (and (action "stop") (eventually stopped?)))
            (define previous-config (contents "/var/lib/gitlab-runner/config.toml"))
            (test-assert "missing secret fails without a leftover process"
              (and (guest '(begin
                             (rename-file "/etc/gitlab-runner-token" "/etc/gitlab-runner-token.saved")
                             #t))
                   (load-script #$(replace-manager failing-manager))
                   (begin
                     (action "start")
                     (sleep 3)
                     (stopped?))))
            (test-equal "failed start preserves config" previous-config
                        (contents "/var/lib/gitlab-runner/config.toml"))
            (test-assert "recovery after failed start"
              (and (guest '(begin
                             (rename-file "/etc/gitlab-runner-token.saved" "/etc/gitlab-runner-token")
                             #t))
                   (load-script #$(replace-manager (manager 1)))
                   (action "enable")
                   (action "start")
                   (eventually running?)))
            (test-assert "stop before activation failure" (and (action "stop") (eventually stopped?)))
            (test-assert "activation failure is contained"
              (guest '(begin
                        (rename-file "/var/lib/gitlab-runner" "/var/lib/gitlab-runner.saved")
                        (call-with-output-file "/var/lib/gitlab-runner" (lambda (port) (display "blocked" port)))
                        (primitive-load #$activation)
                        (file-exists? "/var/lib/gitlab-runner.saved/config.toml"))))
            (test-assert "activation and service recover"
              (and (guest '(begin
                             (delete-file "/var/lib/gitlab-runner")
                             (rename-file "/var/lib/gitlab-runner.saved" "/var/lib/gitlab-runner")
                             (primitive-load #$activation)
                             #t))
                   (action "start")
                   (eventually running?)))
            (test-assert "local coordinator is ready"
              (eventually (lambda () (exists? "/tmp/gitlab-runner-fixture"))))
            (write-file "/tmp/gitlab-runner-fixture/offer" 101)
            (define job-ready? (eventually (lambda () (exists? (job-file 101 "ready")))))
            (test-assert "real shell job starts" job-ready?)
            (unless job-ready?
              (display (contents "/var/log/gitlab-runner.log") (current-error-port))
              (display (contents "/var/log/runner-coordinator.log") (current-error-port))
              (test-end))
            (test-equal "job uses the dedicated account"
              (guest '(passwd:uid (getpwnam "gitlab-runner")))
              (string->number (string-trim-both (contents (job-file 101 "uid")))))
            (test-assert "login-shell job finds service Git and Guix"
              (let ((tools (contents (job-file 101 "tools"))))
                (and (string-contains tools #$(file-append git-minimal "/bin/git"))
                     (string-contains tools #$(file-append guix "/bin/guix")))))
            (test-assert "login-shell job finds extra packages"
              (string-contains (contents (job-file 101 "tools"))
                               #$(file-append hello "/bin/hello")))
            (test-assert "job retains declared environment variables"
              (string-contains (contents (job-file 101 "tools"))
                               "RUNNER_FIXTURE=present"))
            (define job-pid (string->number (string-trim-both (contents (job-file 101 "pid")))))
            (define log-offset (string-length (contents "/var/log/gitlab-runner.log")))
            ;; Fork herd inside the VM so the test can release the job while stop waits.
            (define stop-pid
              (guest '(let ((pid (primitive-fork)))
                        (if (zero? pid)
                            (primitive-exit
                             (if (zero? (system* #$(file-append shepherd "/bin/herd") "stop" "gitlab-runner"))
                                 0
                                 1))
                            pid))))
            (test-assert "stop enters graceful draining with the job still alive"
              (and (eventually
                       (lambda ()
                         (string-contains (substring (contents "/var/log/gitlab-runner.log") log-offset)
                                          "Starting graceful shutdown, waiting for builds to finish")))
                   (running?)
                   (guest `(file-exists? ,(string-append "/proc/" (number->string job-pid))))
                   (not (exists? (job-file 101 "done")))))
            (write-file "/tmp/gitlab-runner-fixture/offer" 102)
            (write-file (fixture-file "release" 101) "release")
            (test-assert "job reports success before manager exits"
              (and (eventually (lambda () (finished? 101 "success")))
                   (eventually stopped?)))
            (test-assert "herd stop completes successfully" (guest `(zero? (cdr (waitpid ,stop-pid)))))
            (test-assert "job and after_script finish without interruption"
              (and (exists? (job-file 101 "done"))
                   (exists? (job-file 101 "after"))
                   (not (exists? (job-file 101 "interrupted")))
                   (not (guest `(file-exists? ,(string-append "/proc/" (number->string job-pid)))))))
            (test-assert "job trace reaches coordinator"
              (string-contains (contents (fixture-file "trace" 101)) "fixture job 101 completed"))
            (test-assert "draining manager leaves next job queued"
              (and (exists? "/tmp/gitlab-runner-fixture/offer")
                   (not (exists? (fixture-file "assigned" 102)))))
            (test-assert "restart picks up queued job"
              (and (action "start")
                   (eventually running?)
                   (eventually (lambda () (exists? (job-file 102 "ready"))))))
            (write-file (fixture-file "release" 102) "release")
            (test-assert "failed job reports failure without stopping manager"
              (and (eventually (lambda () (finished? 102 "failed")))
                   (exists? (job-file 102 "after"))
                   (running?)))
            (test-equal "script failure preserves its exit code" 7
                        (assoc-ref (json-string->scm (contents (fixture-file "result" 102))) "exit_code"))
            (test-assert "coordinator protocol completed without errors"
              (not (exists? "/tmp/gitlab-runner-fixture/protocol-error")))
            (test-assert "final graceful stop" (and (action "stop") (eventually stopped?)))
            (test-end))))))

(define %test-gitlab-runner
  (system-test
    (name "gitlab-runner")
    (description "Runner lifecycle, rollback, real shell jobs and graceful draining against a VM-local coordinator.")
    (value (run-test))))
