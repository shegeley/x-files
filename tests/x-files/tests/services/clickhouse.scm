(define-module (x-files tests services clickhouse)
  #:use-module ((gnu tests)              #:select (simple-operating-system
                                                   marionette-operating-system
                                                   system-test))
  #:use-module ((gnu system vm)          #:select (virtual-machine))
  #:use-module ((gnu services)           #:select (service))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((x-files services clickhouse) #:select (clickhouse-service-type))
  #:use-module ((x-files packages clickhouse) #:select (clickhouse-bin))
  #:use-module (guix gexp)
  #:export (%test-clickhouse))

;;;
;;; ClickHouse System Test
;;;

(define %clickhouse-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service clickhouse-service-type
    `((package    . ,clickhouse-bin)
      (http-port  . "8123")
      (tcp-port   . "9000")
      (listen     . "0.0.0.0")
      (data-dir   . "/var/lib/clickhouse")
      (log-dir    . "/var/log/clickhouse-server")
      (log-level  . "information")
      (log-size   . "500M")
      (log-count  . "3")))))

(define* (run-clickhouse-test #:optional (http-port 8123) (tcp-port 9000))
  "Run tests in %CLICKHOUSE-OS, forwarding HTTP-PORT."
  (define os
    (marionette-operating-system
     %clickhouse-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8123 . ,http-port)
                         (9000 . ,tcp-port)))
     (memory-size 4096)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (ice-9 rdelim)
                       (ice-9 popen))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "clickhouse")

          ;; Wait for the clickhouse service to be up
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'clickhouse)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          ;; Check that log directory was created
          (test-assert "log directory exists"
            (marionette-eval
             '(file-exists? "/var/log/clickhouse-server")
             marionette))

          ;; Check that data directory was created
          (test-assert "data directory exists"
            (marionette-eval
             '(file-exists? "/var/lib/clickhouse")
             marionette))

          ;; Wait for server to accept HTTP connections on port 8123
          (test-assert "http port listening"
            (wait-for-tcp-port #$http-port marionette #:timeout 120))

          ;; Wait for server to accept native TCP connections on port 9000
          (test-assert "tcp port listening"
            (wait-for-tcp-port #$tcp-port marionette #:timeout 30))

          ;; Check that data directory has correct ownership
          (test-assert "data directory owned by clickhouse"
            (marionette-eval
             '(let ((st (stat "/var/lib/clickhouse")))
                (and (file-exists? "/var/lib/clickhouse")
                     (eq? (stat:uid st) (passwd:uid (getpwnam "clickhouse")))
                     (eq? (stat:gid st) (group:gid (getgrnam "clickhouse")))))
             marionette))

          ;; Query via HTTP interface: SELECT 1
          (test-equal "http query SELECT 1"
            "1"
            (marionette-eval
             '(begin
                (use-modules (web client) (web response))
                (catch #t
                  (lambda ()
                    (call-with-values
                        (lambda ()
                          (http-get "http://localhost:8123/?query=SELECT%201"))
                      (lambda (resp body)
                        (if (= (response-code resp) 200)
                            (string-trim-right body)
                            (format #f "HTTP ~a" (response-code resp))))))
                  (lambda (key . args)
                    (format #f "ERROR ~a: ~s" key args))))
             marionette))

          ;; Check server log for successful startup
          (test-assert "server started successfully"
            (marionette-eval
             '(begin
                (use-modules (rnrs io ports))
                (let ((log "/var/log/clickhouse-server/clickhouse-server.log"))
                  (and (file-exists? log)
                       (let ((content (call-with-input-file log get-string-all)))
                         (string-contains content "Ready for connections")))))
             marionette))

          (test-end))))
  (gexp->derivation "clickhouse-test" test))

(define %test-clickhouse
  (system-test
   (name "clickhouse")
   (description "Test a running ClickHouse server: startup, HTTP/TCP ports, and basic query.")
   (value (run-clickhouse-test))))
