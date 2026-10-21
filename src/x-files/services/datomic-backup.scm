(define-module (x-files services datomic-backup)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages java) #:select (openjdk))
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module ((gnu packages databases) #:select (postgresql))
  #:use-module ((gnu packages guile) #:select (guile-json-4))

  #:use-module ((x-files packages datomic) #:select (datomic))

  #:export (datomic-backup-services
            datomic-restore-script))

;;; Restore script — standalone binary, callable from CLI/Emacs/shepherd
;;; Usage:
;;;   datomic-restore list      <db-name>                   — list available backups
;;;   datomic-restore list-json [db-name]                   — same, as JSON on stdout
;;;   datomic-restore datomic <db-name> [backup-dir]        — restore from datomic backup
;;;   datomic-restore pg      <sql-file.gz>                 — restore from pg_dump

(define* (datomic-restore-script
          #:key
          (sql-url "jdbc:postgresql://localhost:5432/datomic")
          (backup-dir "/var/backup/datomic"))
  (with-extensions (list guile-json-4)
    (with-imported-modules (source-module-closure '((json)))
      (program-file "datomic-restore"
        #~(begin
            (use-modules (ice-9 format)
                         (ice-9 ftw)
                         (ice-9 match)
                         (ice-9 popen)
                         (ice-9 rdelim)
                         (srfi srfi-1)
                         (srfi srfi-13)
                         (srfi srfi-19)
                         (json))

            ;; Keep direct invocation independent of a Shepherd environment.
            (setenv "PATH" (string-append #$coreutils "/bin:" #$bash "/bin:"
                                           #$openjdk "/bin"))
            (unless (getenv "XDG_STATE_HOME")
              (setenv "XDG_STATE_HOME"
                      (string-append (passwd:dir (getpwuid (getuid))) "/.local/state")))

            (define datomic-bin (string-append #$datomic "/bin/datomic"))
            (define psql-bin    (string-append #$postgresql "/bin/psql"))
            (define gzip-bin    (string-append #$gzip "/bin/gzip"))

            (define (run! . arguments)
              (unless (zero? (apply system* arguments))
                (error "Datomic restore command failed" (car arguments))))

            (define (list-backups db-name)
              (let ((dir (string-append #$backup-dir "/" db-name)))
                (if (file-exists? dir)
                    (begin
                      (format #t "Datomic backups for '~a':~%" db-name)
                      (run! datomic-bin "list-backups"
                               (string-append "file:" dir))
                      (format #t "~%PostgreSQL dumps:~%")
                      (let ((pg-dir (string-append #$backup-dir "/pg")))
                        (for-each (lambda (f) (format #t "  ~a~%" f))
                                  (or (scandir pg-dir
                                               (lambda (f)
                                                 (string-suffix? ".sql.gz" f)))
                                      '()))))
                    (format #t "No backups found for '~a' in ~a~%"
                            db-name dir))))

            ;; list-backups reads stored transaction points without a live peer.
            ;; Validate its Scheme-readable list, ignoring JVM/logging messages.
            ;; These are incremental backup points, unlike dated pg_dump files.
            (define (backup-t-values dir)
              (if (file-exists? dir)
                  (let* ((port  (open-pipe* OPEN_READ datomic-bin "list-backups"
                                            (string-append "file:" dir)))
                         (lines (let loop ((acc '()))
                                  (let ((line (read-line port)))
                                    (if (eof-object? line)
                                        (reverse acc)
                                        (loop (cons line acc))))))
                         (payload (find (lambda (l)
                                          (string-prefix? "(" (string-trim-both l)))
                                        (reverse lines))))
                    (unless (zero? (close-pipe port))
                      (error "Datomic list-backups failed" dir))
                    (let ((values (and payload (with-input-from-string payload read))))
                      (unless (and (list? values) (every exact-integer? values))
                        (error "Invalid Datomic list-backups output" dir))
                      values))
                  '()))

            (define (iso8601 unix-seconds)
              (date->string (time-utc->date (make-time time-utc 0 unix-seconds) 0)
                            "~Y-~m-~dT~H:~M:~SZ"))

            (define (discover-backup-dbs)
              (filter (lambda (f)
                        (and (not (member f '("." ".." "pg")))
                             (eq? 'directory
                                  (stat:type (stat (string-append #$backup-dir "/" f))))))
                      (or (scandir #$backup-dir) '())))

            (define (pg-dump-entries)
              (let ((dir (string-append #$backup-dir "/pg")))
                (map (lambda (f)
                       (let ((s (stat (string-append dir "/" f))))
                         (list (cons "file" f)
                               (cons "size" (stat:size s))
                               (cons "mtime" (iso8601 (stat:mtime s))))))
                     (sort (or (scandir dir (lambda (f) (string-suffix? ".sql.gz" f)))
                               '())
                           string>?))))

            (define (db-entry db-name)
              (let ((dir (string-append #$backup-dir "/" db-name)))
                (if (file-exists? dir)
                    (let ((t-values (backup-t-values dir)))
                      (list (cons "db" db-name)
                            (cons "backup_dir" dir)
                            (cons "t_values" (list->vector t-values))
                            (cons "latest_t" (if (null? t-values) 'null (apply max t-values)))
                            (cons "count" (length t-values))
                            (cons "dir_mtime" (iso8601 (stat:mtime (stat dir))))))
                    (list (cons "db" db-name)
                          (cons "backup_dir" dir)
                          (cons "error" "no backup directory found")))))

            (define (list-backups-json requested-db)
              (let ((dbs (if requested-db (list requested-db) (discover-backup-dbs))))
                (scm->json
                 (list (cons "backups" (list->vector (map db-entry dbs)))
                       (cons "postgres_dumps" (list->vector (pg-dump-entries))))
                 (current-output-port)
                 #:pretty #t)
                (newline)))

            (define (restore-datomic db-name . rest)
              (let* ((dir (if (null? rest)
                              (string-append #$backup-dir "/" db-name)
                              (car rest)))
                     (uri (string-append "datomic:sql://" db-name
                                         "?" #$sql-url)))
                (format #t "Restoring datomic database '~a' from ~a~%" db-name dir)
                (run! datomic-bin "restore-db" (string-append "file:" dir) uri)
                (format #t "Restore succeeded.~%")))

            (define (restore-pg sql-file)
              (unless (file-exists? sql-file)
                (format #t "File not found: ~a~%" sql-file)
                (exit 1))
              (format #t "Restoring PostgreSQL from ~a~%" sql-file)
              ;; Reject corrupt input before any destructive database command.
              (run! gzip-bin "-t" "--" sql-file)
              (format #t "WARNING: This drops and recreates the datomic database!~%")
              (run! psql-bin "-U" "datomic" "-h" "localhost"
                       "-d" "postgres" "-v" "ON_ERROR_STOP=1"
                       "-c" "DROP DATABASE IF EXISTS datomic;")
              (run! psql-bin "-U" "datomic" "-h" "localhost"
                       "-d" "postgres" "-v" "ON_ERROR_STOP=1"
                       "-c" "CREATE DATABASE datomic;")
              ;; Positional arguments preserve filenames; pipefail checks gzip too.
              (run! #$(file-append bash "/bin/bash") "-o" "pipefail" "-c"
                    "\"$1\" -dc -- \"$2\" | \"$3\" -U datomic -h localhost -d datomic -v ON_ERROR_STOP=1 --single-transaction -f -"
                    "datomic-restore" gzip-bin sql-file psql-bin)
              (format #t "PostgreSQL restore succeeded.~%"))

            (match (cdr (command-line))
              (("list" db-name)
               (list-backups db-name))
              (("list-json")
               (list-backups-json #f))
              (("list-json" db-name)
               (list-backups-json db-name))
              (("datomic" db-name rest ...)
               (apply restore-datomic db-name rest))
              (("pg" sql-file)
               (restore-pg sql-file))
              (_
               (format #t "Usage:
  datomic-restore list      <db-name>            — list available backups
  datomic-restore list-json [db-name]            — same, as JSON on stdout
  datomic-restore datomic <db-name> [backup-dir] — restore from datomic backup
  datomic-restore pg      <sql-file.gz>          — restore from pg_dump

Examples:
  datomic-restore list pitomniki
  datomic-restore list-json
  datomic-restore list-json pitomniki
  datomic-restore datomic pitomniki
  datomic-restore pg /var/backup/datomic/pg/datomic-2026-05-30_030000.sql.gz
")
               (exit 1))))))))

;;; Backup service

(define* (datomic-backup-services
          #:key
          (job-name 'datomic-backup)
          (db-names '())
          (transactor-port "4335")
          (sql-url "jdbc:postgresql://localhost:5432/datomic")
          (backup-dir "/var/backup/datomic")
          (log-file "/var/log/datomic/backup.log")
          (user "datomic")
          (group "datomic")
          ;; daily at 03:00
          (scheduling #~(calendar-event #:hours '(3) #:minutes '(0))))

  (define backup-script
    (program-file "datomic-backup"
      #~(begin
          (use-modules (ice-9 format)
                       (ice-9 ftw)
                       (ice-9 popen)
                       (ice-9 rdelim)
                       (srfi srfi-1)
                       (srfi srfi-13)
                       (srfi srfi-19))

          (define (run! . arguments)
            (unless (zero? (apply system* arguments))
              (error "Datomic backup command failed" (car arguments))))

          ;; Auto-discover Datomic databases via the PEER API
          ;; (datomic.api/get-database-names). Datomic Pro has NO CLI to list
          ;; databases, and the old SQL scan of datomic_kvs.map returned ~50k
          ;; internal storage blobs (not db names) → a `backup-db` JVM per blob →
          ;; the host pinned for hours. `bin/run -e` evals a form with the peer
          ;; classpath; lines are tagged "DBNAME:" so any startup noise is ignored.
          (define (discover-databases)
            (let* ((run-bin     (string-append #$datomic "/bin/run"))
                   (storage-uri (string-append "datomic:sql://*?" #$sql-url))
                   (form (string-append
                          "(do (require '[datomic.api :as d])"
                          " (doseq [n (datomic.api/get-database-names \""
                          storage-uri "\")] (println (str \"DBNAME:\" n)))"
                          " (d/shutdown true) (shutdown-agents))"))
                   (port  (open-pipe* OPEN_READ run-bin "-e" form))
                   (lines (let loop ((acc '()))
                            (let ((line (read-line port)))
                              (if (eof-object? line)
                                  (reverse acc)
                                  (loop (cons (string-trim-both line) acc)))))))
              (unless (zero? (close-pipe port))
                (error "Datomic backup command failed: database discovery"))
              (filter-map
               (lambda (l)
                 (and (string-prefix? "DBNAME:" l)
                      (substring l (string-length "DBNAME:"))))
               lines)))

          (let* ((date-str (date->string (current-date) "~Y-~m-~d_~H~M~S"))
                 (datomic-bin (string-append #$datomic "/bin/datomic"))
                 (pg-dump-bin (string-append #$postgresql "/bin/pg_dump"))
                 (gzip-bin    (string-append #$gzip "/bin/gzip"))
                 (backup-base #$backup-dir)
                 (pg-backup-dir (string-append backup-base "/pg"))
                 ;; empty #:db-names → back up EVERY database in storage
                 (db-names (if (null? '#$db-names)
                               (discover-databases)
                               '#$db-names)))

            ;; 1. PostgreSQL-level backup
            (let ((pg-file (string-append pg-backup-dir "/datomic-" date-str ".sql")))
              (format #t "~a pg_dump → ~a.gz~%" date-str pg-file)
              (run! pg-dump-bin "-U" "datomic" "-h" "localhost"
                    "-f" pg-file "datomic")
              (run! gzip-bin pg-file)

              ;; Keep last 7 pg dumps
              (let ((files (or (scandir pg-backup-dir
                                        (lambda (f) (string-suffix? ".sql.gz" f)))
                               '())))
                (when (> (length files) 7)
                  (for-each (lambda (f)
                              (delete-file (string-append pg-backup-dir "/" f)))
                            (list-tail (sort files string>?) 7)))))

            ;; 2. Datomic-level backups (only for explicitly-listed databases)
            (if (null? db-names)
                (format #t "~a no datomic databases found — datomic-level backup skipped (pg_dump above is a full backup)~%"
                        date-str)
                (begin
                  (format #t "~a databases to backup: ~a~%" date-str db-names)
                  (for-each
                   (lambda (db-name)
                     (let* ((db-backup-dir (string-append backup-base "/" db-name))
                            (uri (string-append
                                  "datomic:sql://" db-name
                                  "?" #$sql-url)))
                       (format #t "~a datomic backup-db ~a → ~a~%"
                               date-str db-name db-backup-dir)
                       (run! datomic-bin "backup-db" uri
                                (string-append "file:" db-backup-dir))))
                   db-names)))

            (format #t "~a backup complete~%" date-str)))))

  (define restore-script*
    (datomic-restore-script #:sql-url sql-url #:backup-dir backup-dir))

  (define activation
    #~(begin
        (use-modules ((guix build utils) #:select (mkdir-p switch-symlinks)))
        (catch #t
          (lambda ()
            (let* ((pw  (getpwnam #$user))
                   (uid (passwd:uid pw))
                   (gid (passwd:gid pw)))
              (for-each (lambda (dir)
                          (mkdir-p dir)
                          (unless (eq? 'directory (stat:type (stat dir)))
                            (error "Datomic backup path is not a directory" dir))
                          (chown dir uid gid))
                        (list #$backup-dir
                              (string-append #$backup-dir "/pg")
                              (dirname #$log-file)))
              (let ((link "/usr/local/bin/datomic-restore"))
                (mkdir-p (dirname link))
                (switch-symlinks link #$restore-script*))))
          (lambda args
            (format (current-error-port) "Datomic backup activation failed: ~s~%" args)
            #t))))

  (define envs
    #~(list
       (string-append "PATH="
                      #$coreutils "/bin:"
                      #$bash "/bin:"
                      #$openjdk "/bin")
       (string-append "DATOMIC_HOME=" #$datomic)
       (string-append "XDG_STATE_HOME=" #$(dirname log-file))
       "TMPDIR=/tmp"))

  (define shepherd-service*
    (shepherd-service
     (provision (list (symbol-append 'datomic-backup '/ job-name)))
     (requirement '(user-processes file-systems networking
                    datomic-postgres-transactor postgres))
     (modules '((shepherd service timer)))
     (start #~(make-timer-constructor
               #$scheduling
               (command
                (list #$backup-script)
                #:user #$user
                #:group #$group
                #:environment-variables #$envs)
               #:log-file #$log-file
               #:wait-for-termination? #t))
     (stop #~(make-timer-destructor))
     (documentation "Datomic database backup (pg_dump + datomic backup-db)")
     (actions
      (list
       (shepherd-action
        (inherit shepherd-trigger-action)
        (documentation "Manually trigger a backup."))
       (shepherd-action
        (name 'list-backups)
        (documentation "List available backups for all configured databases.")
        (procedure
         #~(lambda _
             (for-each
              (lambda (db)
                (system* #$restore-script* "list" db))
              '#$db-names))))))))

  ;; Return list of services
  (list
   (simple-service (symbol-append 'datomic-backup-activation '/ job-name)
                   activation-service-type activation)
   (simple-service (symbol-append 'datomic-backup-shepherd '/ job-name)
                   shepherd-root-service-type (list shepherd-service*))))
