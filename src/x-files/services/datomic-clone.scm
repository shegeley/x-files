(define-module (x-files services datomic-clone)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module ((gnu packages base) #:select (coreutils))

  #:use-module ((x-files packages datomic) #:select (datomic))

  #:export (datomic-clone-script
            datomic-clone-services))

;;; Clone one Datomic database onto another INSIDE THE SAME STORAGE.
;;;
;;; Why this exists: an environment that must show real production data while
;;; being unable to affect production.  Pointing it at the production database
;;; read-only works but leaves every write path dead; pointing it at a periodic
;;; COPY leaves the whole application usable and makes "cannot affect
;;; production" true by construction rather than by discipline.
;;;
;;; Datomic has no "copy database" operation, so the copy is `backup-db`
;;; followed by `restore-db` — both against the same SQL storage, so nothing
;;; leaves the host.  The target is DELETED first: `restore-db` onto a database
;;; whose history has diverged from the backup fails, and a clone is a
;;; replacement, not a merge.
;;;
;;; Peers connected to the TARGET while this runs will see their database
;;; disappear and come back.  That is fine for the preview environments this
;;; serves (they reconnect, or are restarted by their own supervision), and is
;;; the reason the job is scheduled for the small hours by default.
;;;
;;; Usage (values, no records — see the channel's other services):
;;;
;;;   (service datomic-clone-service-type)   ; ← no: this module exports a
;;;                                          ;   services LIST, like
;;;                                          ;   datomic-backup-services, so it
;;;                                          ;   composes into an existing
;;;                                          ;   operating-system services list
;;;
;;;   (append (datomic-clone-services
;;;            #:source-db "pitomniki"
;;;            #:target-db "pitomniki.staging"
;;;            #:password  "…")
;;;           %other-services)

(define (sql-uri db sql-url user password)
  "The datomic:sql:// URI for DB in the storage at SQL-URL.  PASSWORD may be #f
for a trust-authenticated role."
  (string-append "datomic:sql://" db "?" sql-url
                 "?user=" user
                 (if password (string-append "&password=" password) "")))

(define* (datomic-clone-script
          #:key
          (source-db "source")
          (target-db "target")
          (sql-url "jdbc:postgresql://localhost:5432/datomic")
          (db-user "datomic")
          (password #f)
          (work-dir "/var/lib/datomic/clone"))
  "A standalone program that replaces TARGET-DB with a copy of SOURCE-DB.
Callable by hand as well as from the timer below, which is the point: a clone
you can trigger on demand (\"refresh the preview data\") is worth more than one
that only ever happens on a schedule."
  (program-file "datomic-clone"
    #~(begin
        (use-modules (ice-9 format)
                     (ice-9 popen)
                     (ice-9 rdelim)
                     (srfi srfi-19))

        (define datomic-bin (string-append #$datomic "/bin/datomic"))
        (define run-bin     (string-append #$datomic "/bin/run"))
        (define rm-bin      (string-append #$coreutils "/bin/rm"))

        (define backup-dir (string-append #$work-dir "/" #$source-db))
        (define source-uri #$(sql-uri source-db sql-url db-user password))
        (define target-uri #$(sql-uri target-db sql-url db-user password))

        (define (log fmt . args)
          (apply format #t (string-append "~a " fmt "~%")
                 (date->string (current-date) "~Y-~m-~d ~H:~M:~S")
                 args)
          (force-output))

        (define (run! what . argv)
          (log "~a" what)
          (let ((rc (status:exit-val (apply system* argv))))
            (unless (eqv? 0 rc)
              (log "FAILED (~a): ~a" rc what)
              (exit 1))
            rc))

        ;; Datomic Pro has no CLI to drop a database; the peer API does. `bin/run
        ;; -e` evaluates a form with the peer classpath — the same escape hatch
        ;; (x-files services datomic-backup) uses to list databases.
        (define (delete-target!)
          (log "dropping ~a" #$target-db)
          (let* ((form (string-append
                        "(do (require '[datomic.api :as d])"
                        " (println (str \"DELETED:\" (d/delete-database \""
                        target-uri "\"))))"))
                 (port (open-pipe* OPEN_READ run-bin "-e" form))
                 (out  (let loop ((acc '()))
                         (let ((line (read-line port)))
                           (if (eof-object? line)
                               (reverse acc)
                               (loop (cons line acc)))))))
            (close-pipe port)
            (for-each (lambda (l)
                        (when (string-prefix? "DELETED:" l) (log "  ~a" l)))
                      out)))

        (log "clone ~a -> ~a" #$source-db #$target-db)
        ;; A stale backup dir would make restore-db replay an older `t`.
        (run! "clearing previous backup" rm-bin "-rf" backup-dir)
        (run! (string-append "backup-db " #$source-db)
              datomic-bin "backup-db" source-uri (string-append "file:" backup-dir))
        (delete-target!)
        (run! (string-append "restore-db " #$target-db)
              datomic-bin "restore-db" (string-append "file:" backup-dir) target-uri)
        (log "clone complete: ~a is now a copy of ~a" #$target-db #$source-db))))

(define* (datomic-clone-services
          #:key
          (job-name 'datomic-clone)
          (source-db "source")
          (target-db "target")
          (sql-url "jdbc:postgresql://localhost:5432/datomic")
          (db-user "datomic")
          (password #f)
          (work-dir "/var/lib/datomic/clone")
          (log-file "/var/log/datomic/clone.log")
          (user "datomic")
          (group "datomic")
          (requirement '(datomic-postgres-transactor))
          ;; daily at 04:00 — after the 03:00 backup job, so a clone never
          ;; competes with it for the transactor.
          (scheduling #~(calendar-event #:hours '(4) #:minutes '(0))))
  "A shepherd timer that keeps TARGET-DB a copy of SOURCE-DB.  Returns a LIST of
services to splice into an operating-system, mirroring
@code{datomic-backup-services}.  Trigger a clone out of schedule with
@command{herd trigger @var{job-name}}."
  (let ((script (datomic-clone-script #:source-db source-db
                                      #:target-db target-db
                                      #:sql-url   sql-url
                                      #:db-user   db-user
                                      #:password  password
                                      #:work-dir  work-dir)))
    (list
     (simple-service
      (symbol-append job-name '-activation)
      activation-service-type
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils))
            (let* ((pw  (getpwnam #$user))
                   (uid (passwd:uid pw))
                   (gid (passwd:gid pw)))
              (for-each (lambda (dir)
                          (mkdir-p dir)
                          (chown dir uid gid))
                        (list #$work-dir #$(dirname log-file)))))))
     (simple-service
      job-name
      shepherd-root-service-type
      (list
       (shepherd-service
        (provision (list job-name))
        (requirement requirement)
        (documentation "Replace one Datomic database with a copy of another,
inside the same storage.")
        (modules '((shepherd service timer)))
        (start
         #~(make-timer-constructor
            #$scheduling
            (command (list #$script) #:user #$user #:group #$group)
            #:wait-for-termination? #t
            #:log-file #$log-file))
        (stop #~(make-timer-destructor))
        (actions (list shepherd-trigger-action))))))))
