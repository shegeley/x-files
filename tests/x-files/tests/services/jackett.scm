(define-module (x-files tests services jackett)
  #:use-module ((gnu tests)               #:select (simple-operating-system
                                                    marionette-operating-system
                                                    system-test))
  #:use-module ((gnu system vm)           #:select (virtual-machine))
  #:use-module ((gnu services)            #:select (service))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((x-files services jackett) #:select (jackett-configuration
                                                     jackett-service-type))
  #:use-module (guix gexp)
  #:export (%test-jackett))

(define %jackett-port 9117)

(define %jackett-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service jackett-service-type
            (jackett-configuration
             #:data-folder    "/var/lib/jackett"
             #:port           %jackett-port
             ;; Bind all interfaces so the forwarded port is reachable.
             #:listen-public? #t))))

(define* (run-jackett-test #:optional (port %jackett-port))
  "Run tests in %JACKETT-OS, forwarding PORT."
  (define os
    (marionette-operating-system
     %jackett-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,%jackett-port . ,port)))
     ;; The bundled .NET runtime needs headroom.
     (memory-size 2048)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (rnrs io ports))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "jackett")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'jackett))
             marionette))

          (test-assert "jackett listening on port"
            (wait-for-tcp-port #$%jackett-port marionette #:timeout 120))

          (test-assert "data folder exists with correct ownership"
            (marionette-eval
             '(let ((st (stat "/var/lib/jackett"))
                    (pw (getpwnam "jackett")))
                (and (file-exists? "/var/lib/jackett")
                     (= (stat:uid st) (passwd:uid pw))))
             marionette))

          (test-assert "ServerConfig.json written by the daemon"
            (marionette-eval
             '(begin
                (use-modules (ice-9 rdelim))
                (let loop ((i 0))
                  (cond
                   ((file-exists? "/var/lib/jackett/ServerConfig.json") #t)
                   ((> i 60) #f)
                   (else (sleep 1) (loop (+ i 1))))))
             marionette))

          (test-assert "log file created"
            (marionette-eval
             '(file-exists? "/var/log/jackett.log")
             marionette))

          (test-end))))
  (gexp->derivation "jackett-test" test))

(define %test-jackett
  (system-test
   (name "jackett")
   (description "Test a running Jackett tracker-proxy instance.")
   (value (run-jackett-test))))
