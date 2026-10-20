(define-module (x-files tests packages pgbouncer)
  #:use-module ((gnu tests) #:select (simple-operating-system
                                     marionette-operating-system system-test))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((gnu services) #:select (service))
  #:use-module ((gnu services databases)
                #:select (postgresql-service-type postgresql-configuration
                          postgresql-config-file))
  #:use-module ((gnu packages databases) #:select (postgresql))
  #:use-module ((x-files packages pgbouncer) #:select (pgbouncer))
  #:use-module (guix gexp)
  #:export (%test-pgbouncer))

(define vm
  (virtual-machine
   (operating-system
    (marionette-operating-system
     (simple-operating-system
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql)
                (config-file
                 (postgresql-config-file
                  (hba-file
                   (plain-file "pg-hba"
                               "local all all trust\nhost all all 127.0.0.1/32 trust\n")))))))))
   (memory-size 1024)))

(define config-parts
  (list
   "[databases]\nsmoke = host=127.0.0.1 port=5432 dbname=postgres\n"
   "[pgbouncer]\nlisten_addr=127.0.0.1\nlisten_port=6432\n"
   "unix_socket_dir=/tmp\nlogfile=/tmp/pgbouncer.log\n"
   "pidfile=/tmp/pgbouncer.pid\nauth_type=trust\nauth_file="
   (plain-file "pgbouncer-users" "\"postgres\" \"\"\n")
   "\nadmin_users=postgres\npool_mode=transaction\n"))

(define config-without-resolver
  (apply mixed-text-file "pgbouncer-without-resolver.ini" config-parts))

(define config
  (apply mixed-text-file "pgbouncer.ini"
         (append config-parts
                 (list "resolv_conf="
                       (plain-file "pgbouncer-resolv.conf"
                                   "nameserver 127.0.0.1\n")
                       "\n"))))

(define %test-pgbouncer
  (system-test
   (name "pgbouncer")
   (description "Pool PostgreSQL queries and reconnect after a pooler restart.")
   (value
    (with-imported-modules '((gnu build marionette))
      (gexp->derivation
       "pgbouncer-test"
       #~(begin
           (use-modules ((srfi srfi-64) #:select (test-begin test-end test-assert
                                                test-equal test-runner-current))
                        ((gnu build marionette)
                         #:select (make-marionette marionette-eval
                                   wait-for-tcp-port system-test-runner)))
           (define marionette (make-marionette (list #$vm "-cpu" "max")))
           (define (start-pooler)
             (marionette-eval
              '(system* #$(file-append pgbouncer "/bin/pgbouncer")
                        "-u" "postgres" "-d" #$config)
              marionette))
           (define (stop-pooler)
             (marionette-eval
              '(let ((pid (call-with-input-file "/tmp/pgbouncer.pid" read)))
                 (kill pid SIGTERM)
                 (let loop ((remaining 100))
                   (if (not (file-exists? (format #f "/proc/~a" pid)))
                       #t
                       (and (> remaining 0)
                            (begin (usleep 100000) (loop (- remaining 1)))))))
              marionette))
           (define (sql query)
             (marionette-eval
              `(begin
                 (use-modules ((ice-9 popen) #:select (open-pipe* close-pipe))
                              ((ice-9 textual-ports) #:select (get-string-all)))
                 (let* ((pipe (open-pipe*
                               OPEN_READ #$(file-append postgresql "/bin/psql")
                               "-h" "127.0.0.1" "-p" "6432" "-U" "postgres"
                               "-d" "smoke" "-tA" "-v" "ON_ERROR_STOP=1"
                               "-c" ,query))
                        (text (get-string-all pipe)))
                   (and (zero? (close-pipe pipe)) text)))
              marionette))
           (test-runner-current (system-test-runner #$output))
           (test-begin "pgbouncer")
           (test-assert "PostgreSQL ready"
             (wait-for-tcp-port 5432 marionette #:timeout 90))
           (test-assert "resolver failure fixture has no system resolv.conf"
             (marionette-eval
              '(not (file-exists? "/etc/resolv.conf")) marionette))
           (test-equal "missing resolver fails without a signal" 1
             (status:exit-val
              (marionette-eval
               '(system* #$(file-append pgbouncer "/bin/pgbouncer")
                         "-u" "postgres" #$config-without-resolver)
               marionette)))
           (test-assert "resolver failure leaves no pid or socket"
             (marionette-eval
              '(and (not (file-exists? "/tmp/pgbouncer.pid"))
                    (not (file-exists? "/tmp/.s.PGSQL.6432")))
              marionette))
           (test-equal "start pooler" 0 (start-pooler))
           (test-assert "pooler ready"
             (wait-for-tcp-port 6432 marionette #:timeout 30))
           (test-equal "query through pool" "1\n" (sql "SELECT 1"))
           (test-assert "write through pool"
             (sql "CREATE TABLE upgrade_test(value text); INSERT INTO upgrade_test VALUES ('persisted')"))
           (test-assert "stop pooler" (stop-pooler))
           (test-equal "second start" 0 (start-pooler))
           (test-assert "pooler ready after restart"
             (wait-for-tcp-port 6432 marionette #:timeout 30))
           (test-equal "read through restarted pool" "persisted\n"
             (sql "SELECT value FROM upgrade_test"))
           (test-assert "final stop" (stop-pooler))
           (test-assert "bad configuration fails"
             (not (zero?
                   (marionette-eval
                    '(system* #$(file-append pgbouncer "/bin/pgbouncer")
                              "-u" "postgres" "-d" "/missing-config")
                    marionette))))
           (test-assert "failed start leaves no pid file"
             (marionette-eval
              '(not (file-exists? "/tmp/pgbouncer.pid")) marionette))
           (display
            (marionette-eval
             '(begin
                (use-modules ((ice-9 textual-ports) #:select (get-string-all)))
                (if (file-exists? "/tmp/pgbouncer.log")
                    (call-with-input-file "/tmp/pgbouncer.log" get-string-all)
                    "No PgBouncer log was created.\n"))
             marionette))
           (test-end)))))))
