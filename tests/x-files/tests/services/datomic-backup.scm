(define-module (x-files tests services datomic-backup)
  #:use-module ((gnu tests)              #:select (simple-operating-system
                                                   marionette-operating-system
                                                   system-test))
  #:use-module ((gnu system vm)          #:select (virtual-machine))
  #:use-module ((gnu services)           #:select (service
                                                   service-value
                                                   simple-service
                                                   extra-special-file
                                                   activation-service-type))
  #:use-module ((gnu services databases) #:select (postgresql-service-type
                                                   postgresql-configuration
                                                   postgresql-config-file))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service-file))
  #:use-module ((guix scripts system reconfigure) #:select (upgrade-services-program))
  #:use-module ((gnu packages databases)  #:select (postgresql))
  #:use-module ((gnu packages admin)      #:select (shepherd))
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module ((gnu system)              #:select (operating-system
                                                    operating-system-user-services))
  #:use-module ((x-files packages datomic) #:select (datomic))
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
  (define fixture (local-file (search-path %load-path "fixtures/datomic-migration.clj")))
  (define os
    (marionette-operating-system
     %datomic-backup-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((4334 . 4334)))
     (cpu-count 2)
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

          (test-assert "create real Datomic data before backup"
            (marionette-eval
             '(begin
                (setenv "XDG_STATE_HOME" "/tmp/datomic-peer")
                (zero? (system* #$(file-append datomic "/bin/run")
                                #$fixture "create" "migration")))
             marionette))

          (test-assert "pg_dump backup succeeds"
            (marionette-eval
             '(begin
                (zero? (system* #$(file-append postgresql "/bin/pg_dump")
                                "-U" "datomic" "-h" "localhost"
                                "-f" "/var/backup/datomic/pg/test dump.sql"
                                "datomic")))
             marionette))

          (test-assert "pg_dump file exists"
            (marionette-eval
             '(zero? (system* #$(file-append gzip "/bin/gzip")
                              "/var/backup/datomic/pg/test dump.sql"))
             marionette))

          (test-assert "restore script available"
            (marionette-eval
             '(file-exists? "/usr/local/bin/datomic-restore")
             marionette))

          (test-assert "restore refuses to continue when DROP fails on active database"
            (marionette-eval
             '(not (zero? (system* "/usr/local/bin/datomic-restore" "pg"
                                    "/var/backup/datomic/pg/test dump.sql.gz")))
             marionette))

          (test-assert "corrupt archive fails without dropping data"
            (marionette-eval
             '(begin
                (call-with-output-file "/tmp/broken.sql.gz" (lambda (p) (display "not gzip" p)))
                (and (not (zero? (system* "/usr/local/bin/datomic-restore" "pg" "/tmp/broken.sql.gz")))
                     (zero? (system* #$(file-append datomic "/bin/run")
                                     #$fixture "check" "migration" "before"))))
             marionette))

          (test-assert "pg restore: checked helper accepts a path with spaces"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (stop-service 'datomic-postgres-transactor)
                (zero? (system* "/usr/local/bin/datomic-restore" "pg"
                                "/var/backup/datomic/pg/test dump.sql.gz")))
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

          (test-assert "restored Datomic data is queryable"
            (marionette-eval
             '(zero? (system* #$(file-append datomic "/bin/run")
                              #$fixture "check" "migration" "before"))
             marionette))

          (test-assert "SQL restore errors propagate after database recreation"
            (marionette-eval
             '(begin
                (stop-service 'datomic-postgres-transactor)
                (call-with-output-file "/tmp/invalid.sql"
                  (lambda (p) (display "SELECT missing_function();" p)))
                (and (zero? (system* #$(file-append gzip "/bin/gzip") "/tmp/invalid.sql"))
                     (not (zero? (system* "/usr/local/bin/datomic-restore" "pg" "/tmp/invalid.sql.gz")))))
             marionette))
          (test-assert "inspect failed restore and recover from the intact dump"
            (and
             (marionette-eval
              '(begin
                 ;; The failed SQL transaction left a recreated, empty database.
                 (let* ((p (open-pipe* OPEN_READ #$(file-append postgresql "/bin/psql")
                                       "-U" "datomic" "-d" "datomic" "-tA" "-c"
                                       "SELECT count(*) FROM information_schema.tables WHERE table_name = 'datomic_kvs';"))
                        (text (get-string-all p)))
                   (and (zero? (close-pipe p)) (string=? text "0\n")
                        (zero? (system* "/usr/local/bin/datomic-restore" "pg"
                                        "/var/backup/datomic/pg/test dump.sql.gz"))
                        (start-service 'datomic-postgres-transactor))))
              marionette)
             (wait-for-tcp-port 4334 marionette #:timeout 300)
             (marionette-eval
              '(zero? (system* #$(file-append datomic "/bin/run")
                               #$fixture "check" "migration" "before"))
              marionette)))

          (test-end))))

  (gexp->derivation "datomic-backup-test" test))

(define %test-datomic-backup
  (system-test
   (name "datomic-backup")
   (description "Test datomic backup and restore (pg_dump + datomic backup-db).")
   (value (run-datomic-backup-test))))


;;; --- db-names selection + peer auto-discovery --------------------------------
;;;
;;; - explicit #:db-names → backs up exactly those (verbatim, no discovery).
;;; - empty   #:db-names → auto-discovers EVERY database via the peer API
;;;   (datomic.api/get-database-names) and backs each up. This is the correct
;;;   replacement for the old datomic_kvs.map SQL scan, which returned ~50k
;;;   internal storage blobs (not db names) and spawned a backup-db JVM per blob.
;;; The test creates a real database and asserts auto-discovery finds + backs it up.

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
      (datomic-backup-services
       #:job-name 'missing-db
       #:db-names '("does-not-exist")
       #:backup-dir "/var/backup/datomic-missing"
       #:log-file "/var/log/datomic/missing-db.log")
      (operating-system-user-services %backup-base-os)))))

(define (run-datomic-backup-names-test)
  (define fixture (local-file (search-path %load-path "fixtures/datomic-migration.clj")))
  (define query-fixture (local-file (search-path %load-path "fixtures/datomic-read-only.clj")))
  (define restore
    (datomic-restore-script #:sql-url "jdbc:postgresql://localhost:5432/restored?user=datomic"))
  (define restore-schema
    (@@ (x-files services datomic) datomic-sql-migration-create-table))
  (define job-services
    (datomic-backup-services #:job-name 'with-names #:db-names '("testdb")
                            #:log-file "/var/log/datomic/with-names.log"))
  (define activation (program-file "activate-backup" (service-value (car job-services))))
  (define disable
    (upgrade-services-program '() '() '(datomic-backup/with-names) '()))
  (define enable
    (upgrade-services-program
     (map shepherd-service-file (service-value (cadr job-services)))
     '(datomic-backup/with-names) '() '()))
  (define os
    (marionette-operating-system
     %backup-names-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '())
     (cpu-count 2)
     (memory-size 4096)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette (make-marionette (list #$vm)))
          (define herd    #$(file-append shepherd "/bin/herd"))
          (define run-bin #$(file-append datomic "/bin/run"))
          (define sql-url "jdbc:postgresql://localhost:5432/datomic")

          ;; Exercise real data so a backup directory alone cannot pass.
          (define (create-db! name)
            (marionette-eval
             `(begin
                (setenv "XDG_STATE_HOME" "/tmp/datomic-peer")
                (zero? (system* ,run-bin #$fixture "create" ,name)))
             marionette))

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

          (define (restore-and-check! name directory)
            ;; A backup retains its database identity: use separate SQL storage
            ;; with no running transactor, then query it through a read-only peer.
            (marionette-eval
             `(and (zero? (system* #$restore "datomic" ,name ,directory))
                   (zero? (system* ,run-bin #$query-fixture
                                   ,(string-append "datomic:sql://" name
                                     "?read-only=true#jdbc:postgresql://localhost:5432/restored?user=datomic")
                                   "before")))
             marionette))

          (test-runner-current (system-test-runner #$output))
          (test-begin "datomic-backup-names")

          (test-assert "postgresql ready"
            (wait-for-unix-socket "/var/run/postgresql/.s.PGSQL.5432" marionette))

          (test-assert "transactor listening"
            (wait-for-tcp-port 4334 marionette #:timeout 300))

          (test-assert "backup timer stop and second start succeed"
            (marionette-eval
             `(and (zero? (system* ,herd "stop" "datomic-backup/with-names"))
                   (zero? (system* ,herd "start" "datomic-backup/with-names"))
                   (zero? (system* ,herd "status" "datomic-backup/with-names")))
             marionette))
          (test-assert "backup activation handles repetition and a dangling restore link"
            (marionette-eval
             '(begin
                (primitive-load #$activation)
                (delete-file "/usr/local/bin/datomic-restore")
                (symlink "/absent-restore" "/usr/local/bin/datomic-restore")
                (primitive-load #$activation)
                (primitive-load #$activation)
                (file-exists? "/usr/local/bin/datomic-restore"))
             marionette))
          (test-assert "backup activation contains a partial failure and recovers"
            (marionette-eval
             '(begin
                (rename-file "/var/backup/datomic/pg" "/var/backup/datomic/pg.saved")
                (call-with-output-file "/var/backup/datomic/pg" (lambda (p) (display "blocked" p)))
                (primitive-load #$activation)
                (let ((preserved? (eq? 'regular (stat:type (stat "/var/backup/datomic/pg")))))
                  (delete-file "/var/backup/datomic/pg")
                  (rename-file "/var/backup/datomic/pg.saved" "/var/backup/datomic/pg")
                  (primitive-load #$activation)
                  preserved?))
             marionette))

          ;; create a real database so auto-discovery has something to find
          (test-assert "create datomic database 'autodb'"
            (begin (sleep 5) (create-db! "autodb")))
          (test-assert "create explicit database 'testdb'"
            (create-db! "testdb"))

          (test-assert "missing database: backup triggered"
            (trigger! "missing-db"))
          (test-assert "missing database: failure is reported instead of success"
            (let loop ((n 180))
              (cond ((zero? n) #f)
                    ((log-has? "/var/log/datomic/missing-db.log" "backup complete") #f)
                    ((log-has? "/var/log/datomic/missing-db.log" "Datomic backup command failed") #t)
                    (else (sleep 1) (loop (- n 1))))))

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
          (test-assert "backup transaction points can be listed as JSON"
            (marionette-eval '(zero? (system* #$restore "list-json" "testdb")) marionette))
          (test-assert "separate empty restore storage is initialized"
            (marionette-eval
             '(and (zero? (system* #$(file-append postgresql "/bin/psql")
                                   "-U" "datomic" "-d" "postgres" "-v" "ON_ERROR_STOP=1"
                                   "-c" "CREATE DATABASE restored OWNER datomic;"))
                   (zero? (system* #$(file-append postgresql "/bin/psql")
                                   "-U" "datomic" "-d" "restored" "-v" "ON_ERROR_STOP=1"
                                   "-f" #$restore-schema)))
             marionette))
          (test-assert "explicit backup restores queryable data into separate storage"
            (restore-and-check! "restored-explicit" "/var/backup/datomic/testdb"))

          (test-assert "generation service removal and rollback keep backups"
            (marionette-eval
             `(begin
                (primitive-load #$disable)
                (primitive-load #$enable)
                (and (file-exists? "/var/backup/datomic/testdb")
                     (zero? (system* ,herd "status" "datomic-backup/with-names"))))
             marionette))

          ;; --- empty #:db-names '() => peer auto-discovery of every database ---
          (test-assert "auto: backup triggered"
            (trigger! "no-names"))
          (test-assert "auto: backup completes"
            (wait-complete "/var/log/datomic/no-names.log"))
          (test-assert "auto: discovered + backed up 'autodb'"
            (log-has? "/var/log/datomic/no-names.log" "datomic backup-db autodb"))
          (test-assert "auto: datomic backup dir for 'autodb' exists"
            (marionette-eval '(file-exists? "/var/backup/datomic/autodb") marionette))
          (test-assert "auto-discovered backup restores queryable data"
            (restore-and-check! "restored-auto" "/var/backup/datomic/autodb"))
          (test-assert "auto: no 50k-blob explosion (few per-db dirs, not thousands)"
            (marionette-eval
             '(begin (use-modules (ice-9 ftw))
                     (< (length (or (scandir "/var/backup/datomic"
                                             (lambda (f) (not (member f '("." ".." "pg")))))
                                    '()))
                        10))
             marionette))

          (test-end))))

  (gexp->derivation "datomic-backup-names-test" test))

(define %test-datomic-backup-names
  (system-test
   (name "datomic-backup-names")
   (description
    "datomic-backup: explicit #:db-names used verbatim; empty db-names \
auto-discovers every database via the peer API (get-database-names) and backs \
each up — proven by creating a real database in the VM.")
   (value (run-datomic-backup-names-test))))
