(define-module (x-files tests services datomic-clone)
  #:use-module ((gnu tests) #:select (simple-operating-system marionette-operating-system system-test))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((gnu services) #:select (service-value))
  #:use-module ((gnu services shepherd) #:select (shepherd-service-file))
  #:use-module ((gnu packages admin) #:select (shepherd))
  #:use-module ((gnu packages base) #:select (coreutils))
  #:use-module ((guix scripts system reconfigure) #:select (upgrade-services-program))
  #:use-module ((x-files services datomic-clone) #:select (datomic-clone-script datomic-clone-services))
  #:use-module (guix gexp)
  #:export (%test-datomic-clone-safety))

(define (run-test)
  (define services
    (datomic-clone-services
     #:job-name 'clone-safety #:source-db "source" #:target-db "target"
     #:sql-url "jdbc:postgresql://127.0.0.1:9/absent"
     #:work-dir "/var/lib/clone-safety" #:log-file "/var/log/clone-safety/job.log"
     #:user "root" #:group "root" #:requirement '(user-processes)))
  (define script
    (datomic-clone-script #:source-db "source" #:target-db "target"
                         #:sql-url "jdbc:postgresql://127.0.0.1:9/absent"
                         #:work-dir "/var/lib/clone-safety"))
  (define activation (program-file "activate-clone" (service-value (car services))))
  (define disable (upgrade-services-program '() '() '(clone-safety) '()))
  (define enable
    (upgrade-services-program
     (map shepherd-service-file (service-value (cadr services))) '(clone-safety) '() '()))
  (define os
    (marionette-operating-system
     (simple-operating-system (car services) (cadr services))
     #:imported-modules '((gnu services herd) (guix combinators))))
  (define vm (virtual-machine (operating-system os) (memory-size 2048) (cpu-count 2)))
  (gexp->derivation
   "datomic-clone-safety"
   (with-imported-modules '((gnu build marionette))
     #~(begin
         (use-modules ((gnu build marionette) #:select (make-marionette marionette-eval system-test-runner))
                      ((srfi srfi-64) #:select (test-runner-current test-begin test-assert test-end)))
         (define marionette (make-marionette (list #$vm)))
         (define (guest form) (marionette-eval form marionette))
         (define (action verb)
           (guest `(zero? (system* #$(file-append shepherd "/bin/herd") ,verb "clone-safety"))))
         (define (preserved?)
           (guest '(and (file-exists? "/var/lib/clone-safety/source/keep")
                        (string=? "existing backup"
                          (call-with-input-file "/var/lib/clone-safety/source/keep" read-line)))))
         (test-runner-current (system-test-runner #$output))
         (test-begin "datomic-clone-safety")
         (test-assert "timer starts and reports status" (and (action "start") (action "status")))
         (test-assert "existing backup fixture is present"
           (guest '(begin
                     (use-modules ((ice-9 rdelim) #:select (read-line)))
                     (mkdir "/var/lib/clone-safety/source")
                     (call-with-output-file "/var/lib/clone-safety/source/keep"
                       (lambda (port) (display "existing backup" port))) #t)))
         (test-assert "unsupported clone fails without touching existing backups"
           (and (guest '(eqv? 1 (status:exit-val
                                (system* #$(file-append coreutils "/bin/timeout") "45" #$script))))
                (preserved?)))
         (test-assert "timer stop and second start succeed"
           (and (action "stop") (action "start") (action "status")))
         (test-assert "triggered failure leaves data intact and timer stoppable"
           (begin (action "trigger") (and (action "stop") (preserved?))))
         (test-assert "repeated activation preserves state"
           (and (guest '(begin (primitive-load #$activation) (primitive-load #$activation) #t))
                (preserved?)))
         (test-assert "activation contains a blocked directory failure"
           (guest '(begin
                     (rename-file "/var/lib/clone-safety" "/var/lib/clone-safety.saved")
                     (call-with-output-file "/var/lib/clone-safety" (lambda (p) (display "blocked" p)))
                     (primitive-load #$activation)
                     (eq? 'regular (stat:type (stat "/var/lib/clone-safety"))))))
         (test-assert "activation recovery preserves existing backups"
           (and (guest '(begin
                          (delete-file "/var/lib/clone-safety")
                          (rename-file "/var/lib/clone-safety.saved" "/var/lib/clone-safety")
                          (primitive-load #$activation) #t))
                (preserved?)))
         (test-assert "generation removal and restoration preserve state"
           (and (guest '(begin (primitive-load #$disable) (primitive-load #$enable) #t))
                (action "status") (preserved?)))
         (test-assert "second stop is harmless" (and (action "stop") (action "stop")))
         (test-end)))))

(define %test-datomic-clone-safety
  (system-test
    (name "datomic-clone-safety")
    (description "Reject unsupported same-storage clones without data loss; check timer and activation recovery.")
    (value (run-test))))
