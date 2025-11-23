(define-module (x-files tests services datomic)
 #:use-module (gnu tests)
 #:use-module (gnu system)
 #:use-module (gnu packages databases)
 #:use-module (gnu system file-systems)
 #:use-module (gnu system shadow)
 #:use-module (gnu system vm)
 #:use-module (gnu services)
 #:use-module (gnu services databases)
 #:use-module (gnu services networking)
 #:use-module (x-files services datomic)
 #:use-module (guix gexp)
 #:use-module (guix store)
 #:use-module (srfi srfi-1)
 #:export (%test-datomic-dev
           %test-datomic-postgres))

;;;
;;; Datomic Dev Transactor Test
;;;

(define %datomic-dev-os
 (simple-operating-system
  (service dhcpcd-service-type)
  (service datomic-dev-transactor-service-type
   `((port . "4334")
     (log-dir . "/var/log/datomic")))))

(define* (run-datomic-dev-test #:optional (port 4334))
 "Run tests in %DATOMIC-DEV-OS, forwarding PORT."
 (define os
  (marionette-operating-system
   %datomic-dev-os
   #:imported-modules '((gnu services herd)
                        (guix combinators))))

 (define vm
  (virtual-machine
   (operating-system os)
   (port-forwardings `((4334 . ,port)))
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
 test)

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
  (service dhcpcd-service-type)
  (service postgresql-service-type
   (postgresql-configuration
    (postgresql postgresql)))
  (service datomic-postgres-transactor-service-type
   `((port              . "4334")
     (log-dir           . "/var/log/datomic")
     (sql-user          . "datomic")
     (sql-password-file . ,(plain-file "datomic-password" "datomic"))))))

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

      ;; Wait for datomic postgres init to complete
      (test-assert "datomic postgres initialization"
       (marionette-eval
        '(begin
          (use-modules (gnu services herd))
          (start-service 'datomic-postgres-init))
        marionette))

      ;; Start datomic transactor
      (test-assert "datomic transactor service running"
       (marionette-eval
        '(begin
          (use-modules (gnu services herd))
          (start-service 'datomic-transactor))
        marionette))

      ;; Check that PostgreSQL schema was created
      (test-assert "datomic table exists in postgresql"
       (marionette-eval
        '(begin
          (use-modules (ice-9 popen)
           (rnrs io ports))
          (setenv "PGPASSWORD" "datomic")
          (let* ((port (open-pipe*
                        OPEN_READ
                        #$(file-append postgresql "/bin/psql")
                        "-U" "datomic" "-d" "datomic" "-tA" "-c"
                        "SELECT COUNT(*) FROM information_schema.tables WHERE table_name = 'datomic_kvs';"))
                 (output (get-string-all port)))
           (close-pipe port)
           (string=? output "1\n")))
        marionette))

      ;; Wait for transactor to start listening
      (test-assert "transactor listening on port"
       (wait-for-tcp-port #$port marionette))

      ;; Check transactor log for successful startup with PostgreSQL
      (test-assert "transactor started successfully with postgresql"
       (begin
        (sleep 5) ; Give transactor time to start
        (marionette-eval
         '(begin
           (use-modules (rnrs io ports))
           (let ((log-file "/var/log/datomic/transactor.log"))
            (and (file-exists? log-file)
             (let ((content (call-with-input-file log-file get-string-all)))
              (or (string-contains content "protocol=sql")
               (string-contains content "Starting datomic transactor"))))))
         marionette)))

      ;; Verify data directory structure
      (test-assert "datomic directories created"
       (marionette-eval
        '(begin
          (and (file-exists? "/var/lib/datomic")
           (file-exists? "/var/lib/datomic/data")
           (file-exists? "/var/log/datomic")))
        marionette))

      (test-end))))
 test)

(display (run-datomic-postgres-test 5335))
