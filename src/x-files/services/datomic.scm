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

(define-public datomic-transactor-service-type
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
