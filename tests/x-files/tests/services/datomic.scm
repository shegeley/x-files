(define-module (x-files tests services datomic)
 #:use-module ((gnu tests)              #:select (simple-operating-system
                                                  marionette-operating-system
                                                  system-test))
 #:use-module ((gnu system vm)          #:select (virtual-machine))
 #:use-module ((gnu services)           #:select (service
                                                  extra-special-file))
 #:use-module ((gnu services databases) #:select (postgresql-service-type
                                                  postgresql-configuration
                                                  postgresql-config-file))
 #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
 #:use-module ((gnu packages databases) #:select (postgresql))
 #:use-module ((x-files services datomic) #:select (datomic-dev-transactor-service-type
                                                    datomic-postgres-transactor-service-type))
 #:use-module (guix gexp)
 #:export (%test-datomic-dev
           %test-datomic-postgres))

;;;
;;; Datomic Dev Transactor Test
;;;

(define %datomic-dev-os
 (simple-operating-system
  (service dhcpcd-service-type)
  (service datomic-dev-transactor-service-type
   `((port    . "4335")
     (log-dir . "/var/log/datomic")))))

(define* (run-datomic-dev-test #:optional (port 4335))
 "Run tests in %DATOMIC-DEV-OS, forwarding PORT."
 (define os
  (marionette-operating-system
   %datomic-dev-os
   #:imported-modules '((gnu services herd)
                        (guix combinators))))

 (define vm
  (virtual-machine
   (operating-system os)
   (port-forwardings `((4335 . ,port)))
   (memory-size 4024)))  ; Datomic needs more memory

 (define test
  (with-imported-modules '((gnu build marionette))
   #~(begin
      (use-modules (srfi srfi-11) (srfi srfi-64)
       (gnu build marionette)
       (ice-9 rdelim))

      (define marionette
       (make-marionette (list #$vm)))

      (test-runner-current (system-test-runner #$output))
      (test-begin "datomic-dev")

      ;; Wait for datomic transactor to be up and running
      (test-assert "service running"
       (marionette-eval
        '(begin
          (use-modules (gnu services herd))
          (match (start-service 'datomic-transactor)
           (#f #f)
           (('service response-parts ...)
            (match (assq-ref response-parts 'running)
             ((pid) pid)))))
        marionette))

      ;; Check that log directory and files are created
      (test-assert "log directory exists"
       (marionette-eval
        '(file-exists? "/var/log/datomic")
        marionette))

      ;; Wait for transactor to start listening
      (test-assert "transactor listening on port"
       (wait-for-tcp-port #$port marionette))

      ;; Check that data directory is created with correct ownership
      (test-assert "data directory with correct ownership"
       (marionette-eval
        '(begin
          (use-modules (gnu build install))
          (let ((stat (stat "/var/lib/datomic/data")))
           (and (file-exists? "/var/lib/datomic/data")
            (eq? (stat:uid stat) (passwd:uid (getpwnam "datomic")))
            (eq? (stat:gid stat) (group:gid (getgrnam "datomic"))))))
        marionette))

      ;; Check transactor log for successful startup
      (test-assert "transactor started successfully"
       (begin
        (sleep 5) ; Give transactor time to start
        (marionette-eval
         '(begin
           (use-modules (rnrs io ports))
           (let ((log-file "/var/log/datomic/transactor.log"))
            (and (file-exists? log-file)
             (let ((content (call-with-input-file log-file get-string-all)))
              (string-contains content "Launching with Java options")))))
         marionette)))

      (test-end))))
 (gexp->derivation "datomic-dev-test" test))

(define %test-datomic-dev
 (system-test
  (name "datomic-dev")
  (description "Connect to a running Datomic dev transactor.")
  (value (run-datomic-dev-test))))

;;;
;;; Datomic PostgreSQL Transactor Test
;;;

(define %datomic-postgres-os
 (simple-operating-system
  (extra-special-file "/etc/datomic-password"
                      (plain-file "datomic-password" "datomic"))
  (service dhcpcd-service-type)
  (service postgresql-service-type
   (postgresql-configuration
    (postgresql postgresql)
    ;; Allow TCP connections from localhost so Datomic's JDBC URL
    ;; (jdbc:postgresql://localhost:5432/datomic) and test psql queries work.
    (config-file (postgresql-config-file
                  (hba-file (plain-file "pg_hba.conf"
                    "local all all trust\nhost all all 127.0.0.1/32 trust\nhost all all ::1/128 trust\n"))))))
  (service datomic-postgres-transactor-service-type
   `((port              . "4334")
     (log-dir           . "/var/log/datomic")
     (sql-url           . "jdbc:postgresql://localhost:5432/datomic")
     (sql-user          . "datomic")
     (sql-password-file . "/etc/datomic-password")))))

(define* (run-datomic-postgres-test #:optional (port 4334))
 "Run tests in %DATOMIC-POSTGRES-OS, forwarding PORT."
 (define os
  (marionette-operating-system
   %datomic-postgres-os
   #:imported-modules '((gnu services herd) (guix combinators))))

 (define vm
  (virtual-machine
   (operating-system os)
   (port-forwardings `((4334 . ,port)))
   (memory-size 4096)))  ; Datomic + PostgreSQL need more memory

 (define test
  (with-imported-modules '((gnu build marionette))
   #~(begin
      (use-modules
       (srfi srfi-11)
       (srfi srfi-64)
       (gnu build marionette)
       (ice-9 rdelim)
       (ice-9 popen))

      (define marionette
       (make-marionette (list #$vm)))

      (test-runner-current (system-test-runner #$output))
      (test-begin "datomic-postgres")

      ;; Start PostgreSQL first
      (test-assert "postgresql service running"
       (marionette-eval
        '(begin
          (use-modules (gnu services herd))
          (start-service 'postgres))
        marionette))

      ;; Wait for PostgreSQL to be ready
      (test-assert "postgresql ready"
       (wait-for-unix-socket "/var/run/postgresql/.s.PGSQL.5432" marionette))

      ;; Start datomic transactor; shepherd will first start datomic-postgres-init
      ;; (one-shot service that runs psql synchronously to create datomic_kvs table)
      (test-assert "datomic transactor service running"
       (marionette-eval
        '(begin
          (use-modules (gnu services herd))
          (start-service 'datomic-transactor))
        marionette))

      ;; Wait for transactor to start listening.
      ;; JVM startup + Clojure loading + JDBC init in a VM can take several minutes.
      (test-assert "transactor listening on port"
       (wait-for-tcp-port #$port marionette #:timeout 300))

      ;; Check that PostgreSQL schema was created (definitely exists once transactor is up)
      (test-assert "datomic table exists in postgresql"
       (marionette-eval
        '(begin
          (use-modules (ice-9 popen)
           (rnrs io ports))
          (setenv "PGPASSWORD" "datomic")
          (let* ((port (open-pipe*
                        OPEN_READ
                        #$(file-append postgresql "/bin/psql")
                        "-U" "datomic" "-h" "localhost" "-d" "datomic" "-tA" "-c"
                        "SELECT COUNT(*) FROM information_schema.tables WHERE table_name = 'datomic_kvs';"))
                 (output (get-string-all port)))
           (close-pipe port)
           (string=? output "1\n")))
        marionette))

      ;; Check transactor log for successful startup with PostgreSQL.
      ;; Returns full log content as actual-value for diagnostics.
      (test-assert "transactor started successfully with postgresql"
       (marionette-eval
        '(begin
          (use-modules (rnrs io ports))
          (let ((log-file "/var/log/datomic/transactor.log"))
           (and (file-exists? log-file)
            (let ((content (call-with-input-file log-file get-string-all)))
             (and (or (string-contains content "Launching with Java options")
                      (string-contains content "Starting datomic:transactor"))
                  content)))))
        marionette))

      ;; Verify data directory structure
      (test-assert "datomic directories created"
       (marionette-eval
        '(begin
          (and (file-exists? "/var/lib/datomic")
           (file-exists? "/var/lib/datomic/data")
           (file-exists? "/var/log/datomic")))
        marionette))

      (test-end))))
 (gexp->derivation "datomic-postgres-test" test))

(define %test-datomic-postgres
 (system-test
  (name "datomic-postgres")
  (description "Connect to a running Datomic postgres transactor.")
  (value (run-datomic-postgres-test))))
