(define-module (x-files services datomic-backup)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages java) #:select (openjdk))
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module ((gnu packages databases) #:select (postgresql))

  #:use-module ((x-files packages datomic) #:select (datomic))

  #:export (datomic-backup-services
            datomic-restore-script))

;;; Restore script — standalone binary, callable from CLI/Emacs/shepherd
;;; Usage:
;;;   datomic-restore list    <db-name>                     — list available backups
;;;   datomic-restore datomic <db-name> [backup-dir]        — restore from datomic backup
;;;   datomic-restore pg      <sql-file.gz>                 — restore from pg_dump

(define* (datomic-restore-script
          #:key
          (sql-url "jdbc:postgresql://localhost:5432/datomic")
          (backup-dir "/var/backup/datomic"))
  (program-file "datomic-restore"
    #~(begin
        (use-modules (ice-9 format)
                     (ice-9 ftw)
                     (ice-9 match))

        (define datomic-bin (string-append #$datomic "/bin/datomic"))
        (define psql-bin    (string-append #$postgresql "/bin/psql"))
        (define gzip-bin    (string-append #$gzip "/bin/gzip"))

        (define (list-backups db-name)
          (let ((dir (string-append #$backup-dir "/" db-name)))
            (if (file-exists? dir)
                (begin
                  (format #t "Datomic backups for '~a':~%" db-name)
                  (system* datomic-bin "list-backups"
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

        (define (restore-datomic db-name . rest)
          (let* ((dir (if (null? rest)
                          (string-append #$backup-dir "/" db-name)
                          (car rest)))
                 (uri (string-append "datomic:sql://" db-name
                                     "?" #$sql-url)))
            (format #t "Restoring datomic database '~a' from ~a~%" db-name dir)
            (let ((rc (system* datomic-bin "restore-db"
                               (string-append "file:" dir) uri)))
              (if (zero? (status:exit-val rc))
                  (format #t "Restore succeeded.~%")
                  (begin
                    (format #t "Restore FAILED (exit ~a).~%"
                            (status:exit-val rc))
                    (exit 1))))))

        (define (restore-pg sql-file)
          (unless (file-exists? sql-file)
            (format #t "File not found: ~a~%" sql-file)
            (exit 1))
          (format #t "Restoring PostgreSQL from ~a~%" sql-file)
          (format #t "WARNING: This drops and recreates the datomic database!~%")
          (system* psql-bin "-U" "datomic" "-h" "localhost"
                   "-c" "DROP DATABASE IF EXISTS datomic;")
          (system* psql-bin "-U" "datomic" "-h" "localhost"
                   "-c" "CREATE DATABASE datomic;")
          (let ((rc (system (string-append
                             gzip-bin " -dc " sql-file
                             " | " psql-bin
                             " -U datomic -h localhost datomic"))))
            (if (zero? (status:exit-val rc))
                (format #t "PostgreSQL restore succeeded.~%")
                (begin
                  (format #t "PostgreSQL restore FAILED.~%")
                  (exit 1)))))

        (match (cdr (command-line))
          (("list" db-name)
           (list-backups db-name))
          (("datomic" db-name rest ...)
           (apply restore-datomic db-name rest))
          (("pg" sql-file)
           (restore-pg sql-file))
          (_
           (format #t "Usage:
  datomic-restore list    <db-name>              — list available backups
  datomic-restore datomic <db-name> [backup-dir] — restore from datomic backup
  datomic-restore pg      <sql-file.gz>          — restore from pg_dump

Examples:
  datomic-restore list pitomniki
  datomic-restore datomic pitomniki
  datomic-restore pg /var/backup/datomic/pg/datomic-2026-05-30_030000.sql.gz
")
           (exit 1))))))

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
                       (srfi srfi-19))

          (define psql-bin (string-append #$postgresql "/bin/psql"))

          (define (discover-databases)
            "Query postgres for all datomic database names."
            (let* ((port (open-pipe* OPEN_READ
                                     psql-bin
                                     "-U" "datomic" "-h" "localhost"
                                     "-d" "datomic" "-tA" "-c"
                                     "SELECT DISTINCT map FROM datomic_kvs WHERE map NOT LIKE '%$%';"))
                   (output (let loop ((lines '()))
                             (let ((line (read-line port)))
                               (if (eof-object? line)
                                   (reverse lines)
                                   (loop (cons (string-trim-both line)
                                               lines)))))))
              (close-pipe port)
              (filter (lambda (s) (not (string-null? s))) output)))

          (let* ((date-str (date->string (current-date) "~Y-~m-~d_~H~M~S"))
                 (datomic-bin (string-append #$datomic "/bin/datomic"))
                 (pg-dump-bin (string-append #$postgresql "/bin/pg_dump"))
                 (gzip-bin    (string-append #$gzip "/bin/gzip"))
                 (backup-base #$backup-dir)
                 (pg-backup-dir (string-append backup-base "/pg"))
                 (db-names (if (null? '#$db-names)
                               (discover-databases)
                               '#$db-names)))

            ;; 1. PostgreSQL-level backup
            (let ((pg-file (string-append pg-backup-dir "/datomic-" date-str ".sql")))
              (format #t "~a pg_dump → ~a.gz~%" date-str pg-file)
              (let ((rc (system* pg-dump-bin "-U" "datomic" "-h" "localhost"
                                 "-f" pg-file "datomic")))
                (when (zero? (status:exit-val rc))
                  (system* gzip-bin pg-file)))

              ;; Keep last 7 pg dumps
              (let ((files (or (scandir pg-backup-dir
                                        (lambda (f) (string-suffix? ".sql.gz" f)))
                               '())))
                (when (> (length files) 7)
                  (for-each (lambda (f)
                              (delete-file (string-append pg-backup-dir "/" f)))
                            (list-tail (sort files string>?) 7)))))

            ;; 2. Datomic-level backups (per database)
            (format #t "~a databases to backup: ~a~%" date-str db-names)
            (for-each
             (lambda (db-name)
               (let* ((db-backup-dir (string-append backup-base "/" db-name))
                      (uri (string-append
                            "datomic:sql://" db-name
                            "?" #$sql-url)))
                 (format #t "~a datomic backup-db ~a → ~a~%"
                         date-str db-name db-backup-dir)
                 (system* datomic-bin "backup-db" uri
                          (string-append "file:" db-backup-dir))))
             db-names)

            (format #t "~a backup complete~%" date-str)))))

  (define restore-script*
    (datomic-restore-script #:sql-url sql-url #:backup-dir backup-dir))

  (define activation
    #~(begin
        (use-modules (guix build utils))
        (let* ((pw  (getpwnam #$user))
               (uid (passwd:uid pw))
               (gid (passwd:gid pw)))
          (for-each (lambda (dir)
                      (mkdir-p dir)
                      (chown dir uid gid))
                    (list #$backup-dir
                          (string-append #$backup-dir "/pg")
                          (dirname #$log-file)))
          ;; Symlink restore script
          (let ((link "/usr/local/bin/datomic-restore"))
            (mkdir-p (dirname link))
            (when (file-exists? link) (delete-file link))
            (symlink #$restore-script* link)))))

  (define envs
    #~(list
       (string-append "PATH="
                      #$coreutils "/bin:"
                      #$bash "/bin:"
                      #$openjdk "/bin")
       (string-append "DATOMIC_HOME=" #$datomic)
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
