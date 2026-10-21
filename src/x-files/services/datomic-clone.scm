(define-module (x-files services datomic-clone)
  #:use-module (guix gexp)
  #:use-module ((gnu services) #:select (simple-service activation-service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service
                                                  shepherd-root-service-type
                                                  shepherd-trigger-action))

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
  "Return a compatibility command that rejects unsafe same-storage cloning.

Datomic backup/restore preserves database identity and cannot clone within one
SQL storage.  Reject before reading credentials or changing backups or targets.
Existing keyword arguments remain accepted; migration requires separate target
storage and a transactor configured for it."
  (program-file "datomic-clone-disabled"
    #~(begin
        (format (current-error-port)
                "Refusing Datomic clone ~a -> ~a: cloning within the same SQL storage is unsupported. Configure separate target storage; existing data and backups were not changed.~%"
                #$source-db #$target-db)
        (exit 1))))

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
  "Return compatibility services that refuse unsupported same-storage clones.
The timer records a failed attempt without modifying databases or backups.
Migrate callers to separate target storage before implementing a copy job."
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
            (use-modules ((guix build utils) #:select (mkdir-p)))
            (catch #t
              (lambda ()
                (let* ((pw  (getpwnam #$user))
                       (uid (passwd:uid pw))
                       (gid (passwd:gid pw)))
                  (for-each (lambda (dir)
                              (mkdir-p dir)
                              (unless (eq? 'directory (stat:type (stat dir)))
                                (error "Datomic clone path is not a directory" dir))
                              (chown dir uid gid))
                            (list #$work-dir #$(dirname log-file)))))
              (lambda args
                (format (current-error-port) "Datomic clone activation failed: ~s~%" args)
                #t)))))
     (simple-service
      job-name
      shepherd-root-service-type
      (list
       (shepherd-service
        (provision (list job-name))
        (requirement requirement)
        (documentation "Reject unsupported same-storage Datomic clones without changing data.")
        (modules '((shepherd service timer)))
        (start
         #~(make-timer-constructor
            #$scheduling
            (command (list #$script) #:user #$user #:group #$group)
            #:wait-for-termination? #t
            #:log-file #$log-file))
        (stop #~(make-timer-destructor))
        (actions (list shepherd-trigger-action))))))))
