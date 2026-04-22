(define-module (x-files tests services remark42)
  #:use-module ((gnu tests)              #:select (simple-operating-system
                                                   marionette-operating-system
                                                   system-test))
  #:use-module ((gnu system vm)          #:select (virtual-machine))
  #:use-module ((gnu services)           #:select (service))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((x-files services remark42) #:select (remark42-configuration
                                                      remark42-service-type))
  #:use-module (guix gexp)
  #:export (%test-remark42))

(define %remark42-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service remark42-service-type
            (remark42-configuration
             #:url      "http://localhost:8080"
             #:site     "test"
             #:secret   "test-secret-1234"
             #:data-dir "/data/remark42/test"
             #:port     8080))))

(define* (run-remark42-test #:optional (port 8080))
  "Run tests in %REMARK42-OS, forwarding PORT."
  (define os
    (marionette-operating-system
     %remark42-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,port)))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (rnrs io ports))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "remark42")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'remark42))
             marionette))

          (test-assert "remark42 listening on port"
            (wait-for-tcp-port #$port marionette #:timeout 60))

          (test-assert "data directory exists with correct ownership"
            (marionette-eval
             '(begin
                (let* ((stat (stat "/data/remark42/test"))
                       (user (getpwnam "remark42")))
                  (and (file-exists? "/data/remark42/test")
                       (= (stat:uid stat) (passwd:uid user)))))
             marionette))

          (test-assert "backup directory exists"
            (marionette-eval
             '(file-exists? "/data/remark42/test/backup")
             marionette))

          (test-assert "log file created"
            (marionette-eval
             '(file-exists? "/var/log/remark42.log")
             marionette))

          (test-end))))
  (gexp->derivation "remark42-test" test))

(define %test-remark42
  (system-test
   (name "remark42")
   (description "Test a running Remark42 commenting engine instance.")
   (value (run-remark42-test))))
