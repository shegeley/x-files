(define-module (x-files services datomic)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module ((gnu packages databases) #:select (postgresql))
  #:use-module ((gnu services databases) #:select (postgresql-role
                                                   postgresql-role-service-type
                                                   postgresql-service-type
                                                   postgresql-configuration))
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

  #:use-module ((nongnu packages databases) #:select (datomic-cli-tools))
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
  `(("protocol"          . "sql")
    ("sql-url"           . ,(assoc-ref config 'sql-url))
    ("sql-user"          . ,(assoc-ref config 'sql-user))
    ("sql-password-file" . ,(assoc-ref config 'sql-password-file))
    ("host"              . "localhost")
    ("port"              . ,(assoc-ref config 'port))
    ;; not sure if "data-dir" is still actual when using sql protocol
    ("data-dir"          . ,(data-dir config))
    ("log-dir"           . ,(assoc-ref config 'log-dir))))

(define (datomic-postgres-roles config)
 (let* [(username      (assoc-ref config "sql-user"))
        (password-file (assoc-ref config "sql-password-file"))]
  (list
   (postgresql-role
    (name username)
    (permissions '(createdb login))
    (password-file password-file)
    (create-database? #t)))))

;; probably not needed, can be replaced with datomic-postgres-role
#;(define datomic-sql-migration:create-db
   (plain-file "create-datomic-db.sql"
    "CREATE DATABASE IF NOT EXISTS datomic
 WITH OWNER = postgres
        TEMPLATE template0
        ENCODING = 'UTF8'
        TABLESPACE = pg_default
        LC_COLLATE = 'en_US.UTF-8'
        LC_CTYPE = 'en_US.UTF-8'
        CONNECTION LIMIT = -1;"))

(define datomic-sql-migration-create-table
 (plain-file "create-datomic-table.sql"
  "CREATE TABLE IF NOT EXISTS datomic_kvs
(
 id text NOT NULL,
 rev integer,
 map text,
 val bytea,
 CONSTRAINT pk_id PRIMARY KEY (id )
)
WITH (
 OIDS=FALSE
);
ALTER TABLE datomic_kvs
 OWNER TO postgres;
GRANT ALL ON TABLE datomic_kvs TO postgres;
GRANT ALL ON TABLE datomic_kvs TO public;"))

(define (datomic-postgres-init-gexp config)
 #~(begin
    (use-modules (guix build utils))
    (let ((psql #$(file-append postgresql "/bin/psql")))
     (invoke
      psql "-U" "datomic" "-d" "datomic" "-f"
      #$datomic-sql-migration-create-table))))

(define (datomic-postgres-init-script config)
  (program-file "datomic-postgres-init"
                (datomic-postgres-init-gexp config)))

(define (datomic-postgres-init-services config)
  (list
   (shepherd-service
     (provision '(datomic-postgres-init))
     (requirement '(postgres postgres-roles))
     (one-shot? #t)
     (documentation "Initialize Datomic PostgreSQL schema")
     (start #~(make-forkexec-constructor
               (list #$(datomic-postgres-init-script config)))))))

(define (datomic-postgres-shepherd-services config)
  (cons
   (datomic-transactor-shepherd-service config)
   ;; won't work for now, you have to create user by-hand
   #;(datomic-postgres-init-services      config)))

(define-public datomic-postgres-transactor-service-type
  (service-type
    (name 'datomic-transactor)
    (description "Datomic Transactor Service")
    (extensions
     (list
      (service-extension postgresql-role-service-type
                         datomic-postgres-roles)
      (service-extension profile-service-type
                         (const (list datomic datomic-cli-tools)))
      (service-extension account-service-type
                         datomic-accounts)
      (service-extension activation-service-type
                         datomic-transactor-activation)
      (service-extension shepherd-root-service-type
                         datomic-postgres-shepherd-services)))
    (default-value datomic-default-value)))
