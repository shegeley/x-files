(define-module (x-files tests services datomic-migration)
  #:use-module ((gnu tests) #:select (simple-operating-system marionette-operating-system system-test))
  #:use-module ((gnu services) #:select (service extra-special-file))
  #:use-module ((gnu services databases) #:select (postgresql-service-type postgresql-configuration postgresql-config-file))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service shepherd-service-file))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((gnu packages databases) #:select (postgresql))
  #:use-module ((gnu packages admin) #:select (shepherd))
  #:use-module ((guix scripts system reconfigure) #:select (switch-system-program upgrade-services-program))
  #:use-module ((x-files packages datomic) #:select (datomic))
  #:use-module ((x-files services datomic) #:select (datomic-postgres-transactor-service-type))
  #:use-module (guix gexp)
  #:export (%test-datomic-migration))

(define previous-datomic
  ((@@ (x-files packages datomic) make-datomic)
   "1.0.7705" "0zlc3cddh8gcr0i754rj9linswwmf8m06nm3k3cv1pwkp01jcz51"))
(define next-datomic datomic)

(define (config package)
  `((package . ,package)
    (port . "4334")
    (log-dir . "/var/log/datomic")
    (sql-url . "jdbc:postgresql://localhost:5432/datomic")
    (sql-user . "datomic")
    (sql-password-file . "/etc/datomic-password")))

(define (test-os package)
  (marionette-operating-system
   (simple-operating-system
    (service dhcpcd-service-type)
    (extra-special-file "/etc/datomic-password" (plain-file "datomic-password" "datomic"))
    (service postgresql-service-type
      (postgresql-configuration
        (postgresql postgresql)
        (config-file
         (postgresql-config-file
           (hba-file (plain-file "pg_hba.conf"
                       "local all all trust\nhost all all 127.0.0.1/32 trust\nhost all all ::1/128 trust\n"))))))
    (service datomic-postgres-transactor-service-type (config package)))
   #:imported-modules '((gnu services herd) (guix combinators))))

(define (transactor package)
  ((@@ (x-files services datomic) datomic-postgres-transactor-shepherd-service)
   (config package)))

(define (replace-transactor service)
  (upgrade-services-program (list (shepherd-service-file service)) '() '() '(datomic-postgres-transactor)))

(define (run-test)
  (define old-os (test-os previous-datomic))
  (define new-os (test-os next-datomic))
  (define vm (virtual-machine (operating-system old-os) (memory-size 4096) (cpu-count 2)))
  (define fixture (local-file (search-path %load-path "fixtures/datomic-migration.clj")))
  (define activation
    (program-file "activate-datomic"
      ((@@ (x-files services datomic) datomic-transactor-activation) (config next-datomic))))
  (define failed-service
    (shepherd-service
      (inherit (transactor next-datomic))
      (respawn? #f)
      ;; The wrapper creates/checks SQL state before Java rejects this flag.
      (start
       (let ((bad-config (cons '(java-opts . ("-XX:ThisOptionDoesNotExist"))
                               (config next-datomic))))
         ((@ (gnu services shepherd) shepherd-service-start)
          ((@@ (x-files services datomic) datomic-postgres-transactor-shepherd-service)
           bad-config))))))
  (gexp->derivation
   "datomic-migration"
   (with-imported-modules '((gnu build marionette))
     #~(begin
         (use-modules ((gnu build marionette) #:select (make-marionette marionette-eval system-test-runner wait-for-tcp-port wait-for-unix-socket))
                      ((srfi srfi-64) #:select (test-runner-current test-begin test-assert test-equal test-end)))
         (define marionette (make-marionette (list #$vm)))
         (define (guest expression) (marionette-eval expression marionette))
         (define (load-script script) (guest `(begin (primitive-load ,script) #t)))
         (define (action verb)
           (guest `(zero? (system* #$(file-append shepherd "/bin/herd") ,verb "datomic-postgres-transactor"))))
         (define (running-pid)
           (guest '(begin
                     (use-modules ((gnu services herd) #:select (current-services live-service-running live-service-provision))
                                  ((srfi srfi-1) #:select (find)))
                     (let ((service (find (lambda (s) (memq 'datomic-postgres-transactor (live-service-provision s))) (current-services))))
                       (and service (live-service-running service))))))
         (define (listening?)
           (guest '(let ((s (socket AF_INET SOCK_STREAM 0)))
                     (catch 'system-error
                       (lambda () (connect s AF_INET INADDR_LOOPBACK 4334) (close-port s) #t)
                       (lambda _ (close-port s) #f)))))
         (define (stopped?) (and (not (running-pid)) (not (listening?))))
         (define (version-running? package)
           (guest `(begin
                     (use-modules ((ice-9 ftw) #:select (scandir))
                                  ((rnrs io ports) #:select (get-string-all))
                                  ((srfi srfi-1) #:select (count)))
                     (= 1 (count
                           (lambda (pid)
                             (catch 'system-error
                               (lambda ()
                                 (let ((cmd (call-with-input-file (string-append "/proc/" pid "/cmdline") get-string-all)))
                                   (and (string=? (readlink (string-append "/proc/" pid "/cwd")) ,package)
                                        (string-contains cmd "datomic.launcher"))))
                               (lambda _ #f)))
                           (scandir "/proc" (lambda (name) (string->number name))))))))
         (define (ready?) (wait-for-tcp-port 4334 marionette #:timeout 300))
         (define (peer package operation . values)
           (guest `(begin
                     (setenv "XDG_STATE_HOME" "/tmp/datomic-peer")
                     (zero? (system* (string-append ,package "/bin/run") #$fixture
                                     ,operation "migration" ,@values)))))
         (define (eventually predicate)
           (let loop ((left 60))
             (cond ((predicate) #t) ((zero? left) #f)
                   (else (sleep 1) (loop (- left 1))))))
         (test-runner-current (system-test-runner #$output))
         (test-begin "datomic-migration")
         (test-assert "PostgreSQL ready"
           (wait-for-unix-socket "/var/run/postgresql/.s.PGSQL.5432" marionette))
         (test-assert "old transactor starts" (and (ready?) (version-running? #$previous-datomic)))
         (test-assert "old peer writes schema and data" (peer #$previous-datomic "create"))
         (test-assert "old peer reads data" (peer #$previous-datomic "check" "before"))
         (test-assert "stop removes process and listener" (and (action "stop") (eventually stopped?)))
         (test-assert "second stop is harmless" (begin (action "stop") (stopped?)))
         (test-assert "second start preserves data"
           (and (action "start") (ready?) (peer #$previous-datomic "check" "before")))
         (test-assert "activation is repeatable"
           (and (load-script #$activation) (load-script #$activation)))
         (test-assert "switch activates new system and queues service replacement"
           (let ((before (running-pid)))
             (and (load-script #$(switch-system-program new-os))
                  (load-script #$(replace-transactor (transactor next-datomic)))
                  (equal? before (running-pid)))))
         (test-assert "new transactor reads old data"
           (and (action "stop") (eventually stopped?) (action "start") (ready?)
                (version-running? #$next-datomic)
                (peer #$next-datomic "check" "before")))
         (test-assert "old peer can read new transactor" (peer #$previous-datomic "check" "before"))
         (test-assert "new peer adds data" (peer #$next-datomic "add"))
         (test-assert "switch back activates prior package"
           (and (load-script #$(switch-system-program old-os))
                (load-script #$(replace-transactor (transactor previous-datomic)))
                (action "stop") (eventually stopped?) (action "start") (ready?)
                (version-running? #$previous-datomic)))
         (test-assert "old transactor reads data written before and after upgrade"
           (peer #$previous-datomic "check" "before" "after"))
         (test-assert "failed replacement leaves no process or listening port"
           (and (action "stop") (eventually stopped?)
                (load-script #$(replace-transactor failed-service))
                (begin (action "start") (eventually stopped?))))
         (test-assert "failed start preserves SQL storage"
           (guest '(zero? (system* #$(file-append postgresql "/bin/psql")
                                   "-U" "datomic" "-d" "datomic" "-v" "ON_ERROR_STOP=1"
                                   "-c" "SELECT count(*) FROM datomic_kvs;"))))
         (test-assert "recovery after failed start preserves all data"
           ;; Shepherd disables a non-respawning service after unexpected exit;
           ;; a replacement inherits that flag until explicitly enabled.
           (and (action "status")
                (load-script #$(replace-transactor (transactor next-datomic)))
                (action "enable")
                (action "start") (ready?)
                (peer #$next-datomic "check" "before" "after")))
         (test-assert "stop before activation failure test" (and (action "stop") (eventually stopped?)))
         (test-assert "activation failure is contained without deleting data"
           (guest '(begin
                     (rename-file "/var/log/datomic" "/var/log/datomic.saved")
                     (call-with-output-file "/var/log/datomic" (lambda (p) (display "blocked" p)))
                     (primitive-load #$activation)
                     (and (file-exists? "/var/lib/datomic/data")
                          (eq? 'regular (stat:type (stat "/var/log/datomic")))))))
         (test-assert "activation recovers and final start reads preserved data"
           (and (guest '(begin
                          (delete-file "/var/log/datomic")
                          (rename-file "/var/log/datomic.saved" "/var/log/datomic")
                          (primitive-load #$activation) #t))
                (action "start") (ready?) (peer #$next-datomic "check" "before" "after")))
         (test-end)))))

(define %test-datomic-migration
  (system-test
    (name "datomic-migration")
    (description "Exercise SQL data, peer compatibility, 7705/7745 generation switching, failed start, and activation recovery in a VM.")
    (value (run-test))))
