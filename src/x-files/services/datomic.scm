(define-module (x-files services datomic)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu services shepherd) #:select (shepherd-service
                                                  shepherd-root-service-type))
  #:use-module ((gnu system shadow) #:select (account-service-type
                                              user-account
                                              user-group))

  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages java) #:select (openjdk))

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix records)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:use-module ((x-files packages datomic) #:select (datomic)))

(define str string-append)

(define (serialize-transactor-config config)
  (plain-file
   "transactor.properties"
   (fold
    (lambda (acc x) (str acc "\n" x)) ""
    (map (match-lambda ((x . y) (str x "=" y))) config))))

(define (flist x . fs) (map (lambda (f) (f x)) fs))

(define (datomic-accounts config)
  (list
   (user-account
         (name "datomic")
         (group "datomic")
         (system? #t)
         (home-directory "/var/lib/datomic")
         (comment "Datomic transactor user"))
   (user-group (name "datomic"))))

(define (datomic-dir config)
  (user-account-home-directory (first (datomic-accounts config))))

(define (data-dir config)
  (string-append (datomic-dir config) "/data"))

(define (transactor/dev-config config)
  `(("protocol"               . "dev")
    ("host"                   . "localhost")
    ("port"                   . ,(assoc-ref config 'port))
    ("data-dir"               . ,(data-dir config))
    ("log-dir"                . ,(assoc-ref config 'log-dir))
    ;; set carefully. you can get
    ;; 2025-10-31 12:19:48 Terminating process - Error starting transactor
    ;; 2025-10-31 12:19:48 java.lang.IllegalArgumentException: :db.error/not-enough-memory (datomic.objectCacheMax + d
    ;; atomic.memoryIndexMax) exceeds 75% of JVM RAM
    ;; 2025-10-31 12:19:48     at datomic.error$arg.invokeStatic(error.clj:79)
    ;; 2025-10-31 12:19:48     at java.base/java.lang.Thread.run(Unknown Source)
    ;; 2025-10-31 12:20:18 Launching with Java options -server -Xms1g -Xmx1g -XX:+UseG1GC -XX:MaxGCPauseMillis=50
    ("memory-index-threshold" . "32m")
    ("memory-index-max"       . "256m")
    ("object-cache-max"       . "128m")))

(define (log-dir config) (assoc-ref config 'log-dir))

(define (datomic-transactor-shepherd-service config)
  (let* ((props      (transactor/dev-config config))
         (props-file (serialize-transactor-config props)))
    (shepherd-service
     (documentation "Datomic Transactor")
     (provision '(datomic-transactor))
     (requirement '(file-systems networking))
     (start #~(make-forkexec-constructor
               (list (string-append #$datomic "/bin/transactor") #$props-file)
               #:user "datomic"
               #:group "datomic"
               ;; #:pid-file "/var/run/datomic-transactor.pid"
               #:log-file (string-append #$(log-dir config) "/transactor.log")
               #:environment-variables
               (list
                (string-append "PATH="
                               (string-append #$coreutils "/bin") ":"
                               (string-append #$bash "/bin") ":"
                               (string-append #$openjdk "/bin"))
                (string-append "DATOMIC_HOME="   #$datomic)
                (string-append "XDG_STATE_HOME=" #$(data-dir config)))))
     (stop #~(make-kill-destructor)))))

(define (datomic-transactor-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$(data-dir config))
      (mkdir-p #$(log-dir config))
      (chown "datomic:datomic"
             #$(datomic-dir config)
             #$(log-dir config))))

(define (datomic-shepherd-services config)
  (list (datomic-transactor-shepherd-service config)))

(define datomic-default-value
  `((port     . "4334")
    (log-dir  . "/var/log/datomic")))

(define-public datomic-dev-transactor-service-type
  (service-type
   (name 'datomic-transactor)
   (description "Datomic Transactor Service")
   (extensions
    (list
     (service-extension profile-service-type       (const (list datomic)))
     (service-extension account-service-type       datomic-accounts)
     (service-extension activation-service-type    datomic-transactor-activation)
     (service-extension shepherd-root-service-type datomic-shepherd-services)))
   (default-value datomic-default-value)))

;; POSTGRES [DRAFT]

(define (transactor/postgres-config config)
  `(("protocol" . "sql")
    ("sql-url" . ,(assoc-ref config 'sql-url))
    ("sql-user" . ,(assoc-ref config 'sql-user))
    ("sql-password" . ,(assoc-ref config 'sql-password))
    ("host" . "localhost")
    ("port" . ,(assoc-ref config 'port))
    ("data-dir" . ,(data-dir config))
    ("log-dir" . ,(assoc-ref config 'log-dir))
    ("memory-index-threshold" . "32m")
    ("memory-index-max" . "256m")
    ("object-cache-max" . "128m")))

(define datomic-postgres-role
  (postgresql-role
   (name "datomic")
   (permissions '(createdb login))
   (create-database? #t)))

(define (datomic-postgres-init-gexp config)
  ;; TODO: modify all schemas: ADD CREATE IF NOT EXISTS
  ;; TODO: pass schemas as config
  #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 rdelim))

      ;; Find the create-schema.sql file in the datomic package
      (let* ((datomic-home #$datomic)
             (sql-file (string-append datomic-home "/sql/create-schema.sql"))
             (psql #$(file-append postgresql "/bin/psql"))
             (db-uri #$(getenv "DATOMIC_DB_URI")))

        (when (file-exists? sql-file)
          (let ((port (open-pipe* OPEN_READ psql "-d" db-uri "-f" sql-file)))
            (let loop ()
              (let ((line (read-line port)))
                (when (not (eof-object? line))
                  (display line)
                  (newline)
                  (loop))))
            (close-pipe port))))))

(define (datomic-postgres-init-script config)
  (program-file "datomic-postgres-init"
                (datomic-postgres-init-gexp config)))

(define (datomic-postgres-init-services _)
  (list
   (shepherd-service
     (provision '(datomic-postgres-init))
     (requirement '(postgres postgres-roles))
     (one-shot? #t)
     (documentation "Initialize Datomic PostgreSQL schema")
     (start #~(make-forkexec-constructor
               (list #$(datomic-postgres-init-script))
               #:environment-variables
               (list (string-append "DATOMIC_DB_URI=postgresql://datomic@localhost/datomic")))))))

;; Updated Datomic configuration for PostgreSQL
(define (transactor/postgres-config config)
  `(("protocol" . "sql")
    ("sql-url"  . ,(or (assoc-ref config 'sql-url) "datomic"))
    ("sql-user" . ,(or (assoc-ref config 'sql-user) "datomic"))
    ("sql-password" . ,(or (assoc-ref config 'sql-password) "datomic"))
    ("host" . "localhost")
    ("port" . ,(assoc-ref config 'port))
    ("data-dir" . ,(data-dir config))
    ("log-dir" . ,(assoc-ref config 'log-dir))
    ("memory-index-threshold" . "32m")
    ("memory-index-max" . "256m")
    ("object-cache-max" . "128m")))

(define (datomic-postgres-shepherd-services config)
  (cons
   (datomic-transactor-shepherd-service config)
   (datomic-postgres-init-services      config)))

(define-public datomic-postgres-transactor-service-type
  (service-type
    (name 'datomic-transactor)
    (description "Datomic Transactor Service")
    (extensions
     (list
      (service-extension postgresql-role-service-type
                         (const (list datomic-postgres-role)))
      (service-extension profile-service-type
                         (const (list datomic)))
      (service-extension account-service-type
                         datomic-accounts)
      (service-extension activation-service-type
                         datomic-transactor-activation)
      (service-extension shepherd-root-service-type
                         datomic-postgres-shepherd-services)))
    (default-value datomic-default-value)))
