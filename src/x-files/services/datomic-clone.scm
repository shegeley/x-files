(define-module (x-files services datomic-clone)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)

  #:use-module ((gnu packages base) #:select (coreutils))

  #:use-module ((x-files packages datomic) #:select (datomic))

  #:export (datomic-clone-script
            datomic-clone-services))

(define* (datomic-clone-script
          #:key
          (source-db "source")
          (target-db "target")
          (sql-url "jdbc:postgresql://localhost:5432/datomic")
          (db-user "datomic")
          (password-file #f)
          (work-dir "/var/lib/datomic/clone"))
  "A standalone program that replaces TARGET-DB with a copy of SOURCE-DB.

PASSWORD-FILE is read at RUN time, never inlined: a store file is world
readable, so a password spliced into this gexp would be a password published to
every user on the machine.  #f for a trust-authenticated role.

Datomic has no copy operation, so a clone is `backup-db' then `restore-db',
both against the same storage -- nothing leaves the host.  The target is
DELETED first: restore-db onto a database whose history has diverged fails, and
a clone is a replacement, not a merge.  Peers connected to the target see it
disappear and come back.

Callable by hand as well as from the timer, which is the point: a clone you can
trigger on demand is worth more than one that only happens on a schedule."
  (program-file "datomic-clone"
    #~(begin
        (use-modules (ice-9 format)
                     (ice-9 popen)
                     (ice-9 rdelim)
                     (srfi srfi-13)
                     (srfi srfi-19))

        (define datomic-bin (string-append #$datomic "/bin/datomic"))
        (define run-bin     (string-append #$datomic "/bin/run"))
        (define rm-bin      (string-append #$coreutils "/bin/rm"))

        (define backup-dir (string-append #$work-dir "/" #$source-db))

        (define (password)
          (let ((f #$(or password-file #f)))
            (and f (file-exists? f)
                 (string-trim-both (call-with-input-file f read-line)))))

        (define (uri db)
          (string-append "datomic:sql://" db "?" #$sql-url "?user=" #$db-user
                         (let ((p (password)))
                           (if p (string-append "&password=" p) ""))))

        (define source-uri (uri #$source-db))
        (define target-uri (uri #$target-db))

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

        (define (delete-target!)
          "Datomic Pro has no CLI to drop a database, so evaluate the peer call
with `bin/run -e' -- the escape hatch datomic-backup already uses."

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
          (password-file #f)
          (work-dir "/var/lib/datomic/clone")
          (log-file "/var/log/datomic/clone.log")
          (user "datomic")
          (group "datomic")
          (requirement '(datomic-postgres-transactor))
          ;; 04:00: after the 03:00 backup, so the two never compete.
          (scheduling #~(calendar-event #:hours '(4) #:minutes '(0))))
  "A shepherd timer that keeps TARGET-DB a copy of SOURCE-DB.  Returns a LIST of
services to splice into an operating-system, mirroring
@code{datomic-backup-services}.  Trigger a clone out of schedule with
@command{herd trigger @var{job-name}}.

For environments that must show real production data while being unable to
affect it: reading production read-only leaves every write path dead, a copy
leaves the application whole."
  (let ((script (datomic-clone-script #:source-db source-db
                                      #:target-db target-db
                                      #:sql-url   sql-url
                                      #:db-user   db-user
                                      #:password-file password-file
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
