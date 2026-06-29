(define-module (x-files tests services stalwart)
  #:use-module ((gnu tests)               #:select (simple-operating-system
                                                    marionette-operating-system
                                                    system-test))
  #:use-module ((gnu system vm)           #:select (virtual-machine))
  #:use-module ((gnu services)            #:select (service))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))
  #:use-module ((x-files services stalwart) #:select (stalwart-service-type
                                                      stalwart-configuration))
  #:use-module (guix gexp)
  #:export (%test-stalwart))

;;;
;;; stalwart system test.
;;;
;;; Boots an OS running the stalwart service with the default (bootstrap-mode)
;;; configuration: no config.json is pre-written, so Stalwart 0.16 starts in
;;; bootstrap mode, logs a one-time temporary admin password and opens HTTP on
;;; port 8080 for the setup wizard.  The test asserts the daemon started, that
;;; it reached bootstrap mode (a log line emitted before the network-dependent
;;; WebUI download, so it is robust even when the build VM is offline) and that
;;; the bootstrap HTTP listener accepts connections.
;;;

(define %http-port 8080)

(define %stalwart-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service stalwart-service-type
            (stalwart-configuration
             #:recovery-admin "admin:test-secret"))))

(define* (run-stalwart-test #:optional (port %http-port))
  (define os
    (marionette-operating-system
     %stalwart-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 2048)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "stalwart")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'stalwart)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)
                     (x x)))))
             marionette))

          ;; The "bootstrap mode" line is logged before Stalwart attempts to
          ;; download the WebUI bundle, so this holds even with no internet.
          (test-assert "reached bootstrap mode"
            (marionette-eval
             '(begin
                (use-modules (ice-9 textual-ports) (srfi srfi-13))
                (let loop ((tries 0))
                  (let ((log (if (file-exists? "/var/log/stalwart.log")
                                 (call-with-input-file "/var/log/stalwart.log"
                                   get-string-all)
                                 "")))
                    (cond
                     ((string-contains log "bootstrap mode") #t)
                     ((>= tries 60) #f)
                     (else (sleep 1) (loop (+ tries 1)))))))
             marionette))

          (test-assert "bootstrap http port listening"
            (wait-for-tcp-port #$port marionette #:timeout 180))

          (test-end))))

  (gexp->derivation "stalwart-test" test))

(define %test-stalwart
  (system-test
   (name "stalwart")
   (description "Boot the Stalwart mail server in bootstrap mode and check that
the daemon starts, reaches bootstrap mode and opens its HTTP setup port.")
   (value (run-stalwart-test))))
