(define-module (x-files tests services datomic-backup)
  #:use-module ((gnu tests)              #:select (simple-operating-system
                                                   marionette-operating-system
                                                   system-test))
  #:use-module ((gnu system vm)          #:select (virtual-machine))
  #:use-module ((gnu services)           #:select (service
                                                   simple-service
                                                   extra-special-file
                                                   activation-service-type))
  #:use-module ((gnu services databases) #:select (postgresql-service-type
                                                   postgresql-configuration
                                                   postgresql-config-file))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((gnu packages databases)  #:select (postgresql))
  #:use-module ((gnu packages admin)      #:select (shepherd))
  #:use-module ((gnu system)              #:select (operating-system
                                                    operating-system-user-services))
  #:use-module ((x-files services datomic) #:select (datomic-postgres-transactor-service-type))
  #:use-module ((x-files services datomic-backup) #:select (datomic-restore-script
                                                            datomic-backup-services))
  #:use-module (guix gexp)
  #:export (%test-datomic-backup
            %test-datomic-backup-names))

(define %pg-hba
  (plain-file "pg_hba.conf"
    "local all all trust
host all all 127.0.0.1/32 trust
host all all ::1/128 trust
"))

(define %datomic-backup-os
  (simple-operating-system
   (extra-special-file "/etc/datomic-password"
                       (plain-file "datomic-password" "datomic"))
   (service dhcpcd-service-type)
   (service postgresql-service-type
     (postgresql-configuration
      (postgresql postgresql)
      (config-file (postgresql-config-file (hba-file %pg-hba)))))
   (service datomic-postgres-transactor-service-type
     `((port              . "4334")
       (log-dir           . "/var/log/datomic")
       (sql-url           . "jdbc:postgresql://localhost:5432/datomic")
       (sql-user          . "datomic")
       (sql-password-file . "/etc/datomic-password")))
   (simple-service 'test-backup-activation activation-service-type
     #~(begin
         (use-modules (guix build utils))
         (let* ((pw  (getpwnam "datomic"))
                (uid (passwd:uid pw))
                (gid (passwd:gid pw)))
           (for-each (lambda (dir)
                       (mkdir-p dir)
                       (chown dir uid gid))
                     '("/var/backup/datomic"
                       "/var/backup/datomic/pg"
                       "/var/log/datomic")))))
   (extra-special-file "/usr/local/bin/datomic-restore"
                       (datomic-restore-script))))

(define (run-datomic-backup-test)
  (define os
    (marionette-operating-system
     %datomic-backup-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((4334 . 4334)))
     (memory-size 4096)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (ice-9 rdelim)
                       (ice-9 popen)
                       (rnrs io ports))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "datomic-backup")

          (test-assert "postgresql ready"
            (wait-for-unix-socket "/var/run/postgresql/.s.PGSQL.5432"
                                  marionette))

          (test-assert "transactor listening"
            (wait-for-tcp-port 4334 marionette #:timeout 300))

          (test-assert "pg_dump backup succeeds"
            (marionette-eval
             '(begin
                (zero? (system* #$(file-append postgresql "/bin/pg_dump")
                                "-U" "datomic" "-h" "localhost"
                                "-f" "/var/backup/datomic/pg/test-dump.sql"
                                "datomic")))
             marionette))

          (test-assert "pg_dump file exists"
            (marionette-eval
             '(file-exists? "/var/backup/datomic/pg/test-dump.sql")
             marionette))

          (test-assert "restore script available"
            (marionette-eval
             '(file-exists? "/usr/local/bin/datomic-restore")
             marionette))

          (test-assert "pg restore: drop and recreate"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (stop-service 'datomic-postgres-transactor)
                (system* #$(file-append postgresql "/bin/psql")
                         "-U" "datomic" "-h" "localhost"
                         "-c" "DROP DATABASE IF EXISTS datomic;")
                (system* #$(file-append postgresql "/bin/psql")
                         "-U" "datomic" "-h" "localhost"
                         "-c" "CREATE DATABASE datomic;")
                (zero? (system* #$(file-append postgresql "/bin/psql")
                                "-U" "datomic" "-h" "localhost"
                                "-d" "datomic"
                                "-f" "/var/backup/datomic/pg/test-dump.sql")))
             marionette))

          (test-assert "datomic_kvs table exists after pg restore"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen) (rnrs io ports))
                (let* ((port (open-pipe*
                              OPEN_READ
                              #$(file-append postgresql "/bin/psql")
                              "-U" "datomic" "-h" "localhost"
                              "-d" "datomic" "-tA" "-c"
                              "SELECT COUNT(*) FROM information_schema.tables WHERE table_name = 'datomic_kvs';"))
                       (output (get-string-all port)))
                  (close-pipe port)
                  (string=? (string-trim-right output) "1")))
             marionette))

          (test-assert "transactor restarts after restore"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'datomic-postgres-transactor))
             marionette))

          (test-assert "transactor listening after restore"
            (wait-for-tcp-port 4334 marionette #:timeout 300))

          (test-end))))

  (gexp->derivation "datomic-backup-test" test))

(define %test-datomic-backup
  (system-test
   (name "datomic-backup")
   (description "Test datomic backup and restore (pg_dump + datomic backup-db).")
   (value (run-datomic-backup-test))))


;;; --- db-names selection (regression for the 50k-blob runaway) -----------------
;;;
;;; The backup must use #:db-names verbatim and NEVER auto-discover databases by
;;; querying datomic_kvs.map (which holds ~50k internal storage blobs, not db
;;; names — that bug spawned a `datomic backup-db` JVM per blob and pinned the
;;; host). With explicit names it backs up exactly those; with empty names it
;;; does pg_dump only and skips the per-db loop entirely.

(define %backup-base-os
  (simple-operating-system
   (extra-special-file "/etc/datomic-password"
                       (plain-file "datomic-password" "datomic"))
   (service dhcpcd-service-type)
   (service postgresql-service-type
     (postgresql-configuration
      (postgresql postgresql)
      (config-file (postgresql-config-file (hba-file %pg-hba)))))
   (service datomic-postgres-transactor-service-type
     `((port              . "4334")
       (log-dir           . "/var/log/datomic")
       (sql-url           . "jdbc:postgresql://localhost:5432/datomic")
       (sql-user          . "datomic")
       (sql-password-file . "/etc/datomic-password")))))

(define %backup-names-os
  (operating-system
    (inherit %backup-base-os)
    (services
     (append
      (datomic-backup-services
       #:job-name        'with-names
       #:db-names        '("testdb")
       #:transactor-port "4334"
       #:sql-url         "jdbc:postgresql://localhost:5432/datomic"
       #:log-file        "/var/log/datomic/with-names.log")
      (datomic-backup-services
       #:job-name        'no-names
       #:db-names        '()
       #:transactor-port "4334"
       #:sql-url         "jdbc:postgresql://localhost:5432/datomic"
       #:log-file        "/var/log/datomic/no-names.log")
      (operating-system-user-services %backup-base-os)))))

(define (run-datomic-backup-names-test)
  (define os
    (marionette-operating-system
     %backup-names-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '())
     (memory-size 4096)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette (make-marionette (list #$vm)))
          (define herd #$(file-append shepherd "/bin/herd"))

          (define (log-has? path needle)
            (marionette-eval
             `(begin
                (use-modules (srfi srfi-13) (rnrs io ports))
                (and (file-exists? ,path)
                     (call-with-input-file ,path
                       (lambda (p)
                         (if (string-contains (get-string-all p) ,needle) #t #f)))))
             marionette))

          (define (trigger! job)
            (marionette-eval
             `(begin (system (string-append ,herd " trigger datomic-backup/" ,job
                                            " >/dev/null 2>&1 &"))
                     #t)
             marionette))

          (define (wait-complete path)
            (let loop ((n 180))
              (cond ((zero? n) #f)
                    ((log-has? path "backup complete") #t)
                    (else (sleep 1) (loop (- n 1))))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "datomic-backup-names")

          (test-assert "postgresql ready"
            (wait-for-unix-socket "/var/run/postgresql/.s.PGSQL.5432" marionette))

          (test-assert "transactor listening"
            (wait-for-tcp-port 4334 marionette #:timeout 300))

          ;; --- explicit #:db-names '("testdb") ---
          (test-assert "with-names: backup triggered"
            (trigger! "with-names"))
          (test-assert "with-names: backup completes"
            (wait-complete "/var/log/datomic/with-names.log"))
          (test-assert "with-names: pg_dump produced a .sql.gz"
            (marionette-eval
             '(begin (use-modules (ice-9 ftw) (srfi srfi-13))
                     (pair? (or (scandir "/var/backup/datomic/pg"
                                         (lambda (f) (string-suffix? ".sql.gz" f)))
                                '())))
             marionette))
          (test-assert "with-names: backs up exactly (testdb) — no discovery"
            (log-has? "/var/log/datomic/with-names.log" "databases to backup: (testdb)"))
          (test-assert "with-names: ran `datomic backup-db testdb`"
            (log-has? "/var/log/datomic/with-names.log" "datomic backup-db testdb"))

          ;; --- empty #:db-names '() => pg_dump only, NO per-db loop ---
          (test-assert "no-names: backup triggered"
            (trigger! "no-names"))
          (test-assert "no-names: backup completes"
            (wait-complete "/var/log/datomic/no-names.log"))
          (test-assert "no-names: datomic-level backup skipped (the fix)"
            (log-has? "/var/log/datomic/no-names.log" "datomic-level backup skipped"))
          (test-assert "no-names: NO `datomic backup-db` (no 50k-blob runaway)"
            (not (log-has? "/var/log/datomic/no-names.log" "datomic backup-db")))

          (test-end))))

  (gexp->derivation "datomic-backup-names-test" test))

(define %test-datomic-backup-names
  (system-test
   (name "datomic-backup-names")
   (description
    "datomic-backup uses #:db-names verbatim (no datomic_kvs auto-discovery); \
empty db-names does pg_dump only and skips the per-db backup loop.")
   (value (run-datomic-backup-names-test))))
