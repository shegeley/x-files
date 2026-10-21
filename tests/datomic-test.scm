(use-modules ((ares suitbl) #:select (suite test is current-test-runner get-state make-suitbl))
             ((ares suitbl state) #:select (get-run-history))
             ((srfi srfi-1) #:select (every))
             ((gnu services shepherd) #:select (shepherd-service-start))
             ((gnu packages base) #:select (coreutils))
             ((guix store) #:select (with-store run-with-store))
             (guix gexp)
             ((x-files services datomic) #:select (datomic-postgres-transactor-service-type)))

(define runner (make-suitbl))
(current-test-runner runner)

(define (environment config)
  (let* ((service ((@@ (x-files services datomic)
                      datomic-postgres-transactor-shepherd-service)
                  (append config
                          '((port . "4334")
                            (log-dir . "/var/log/datomic")
                            (sql-url . "jdbc:postgresql://localhost/datomic")
                            (sql-user . "datomic")
                            (sql-password-file . "/etc/datomic-password")))))
         (expression (lowered-gexp-sexp
                      (with-store store
                        (run-with-store store
                          (lower-gexp (shepherd-service-start service))))))
         (module (make-module)))
    (module-use! module (resolve-interface '(guile)))
    ;; Evaluate the generated constructor without launching a process.
    (module-define! module 'make-forkexec-constructor
      (lambda* (command #:key user group log-file environment-variables)
        environment-variables))
    (eval expression module)))

(suite "Datomic optional metrics environment"
  (test "an explicit package controls DATOMIC_HOME" ()
    (is (member (string-append "DATOMIC_HOME="
                              (lowered-gexp-sexp
                               (with-store store
                                 (run-with-store store
                                   (lower-gexp #~#$coreutils)))))
                (environment `((package . ,coreutils))))))
  (test "neither metrics setting produces an empty expression" ()
    (is (= 3 (length (environment '())))))
  (test "port alone is valid" ()
    (is (member "DATOMIC_METRICS_PORT=9404"
                (environment '((metrics-port . "9404"))))))
  (test "classpath alone is valid" ()
    (is (member "DATOMIC_EXT_CLASSPATH=/tmp/callback"
                (environment '((metrics-classpath . "/tmp/callback"))))))
  (test "both settings preserve a lowerable classpath" ()
    (let ((env (environment `((metrics-port . "9404")
                             (metrics-classpath . ,(file-append coreutils "/share"))))))
      (is (= 5 (length env)))
      (is (member "DATOMIC_METRICS_PORT=9404" env))
      (is (string-prefix? "DATOMIC_EXT_CLASSPATH=/gnu/store/" (list-ref env 3))))))

(let ((history (get-run-history (get-state runner))))
  (exit (if (and (pair? history)
                 (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome))) history))
            0 1)))
