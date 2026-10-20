(define-module (x-files tests packages etcd)
  #:use-module ((gnu tests) #:select (simple-operating-system
                                     marionette-operating-system system-test))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((x-files packages etcd) #:select (etcd))
  #:use-module (guix gexp)
  #:export (%test-etcd))

(define vm
  (virtual-machine
   (operating-system
    (marionette-operating-system (simple-operating-system)))
   (memory-size 1024)))

(define %test-etcd
  (system-test
   (name "etcd")
   (description "Exercise an isolated etcd member and persistence across restart.")
   (value
    (with-imported-modules '((gnu build marionette))
      (gexp->derivation
       "etcd-test"
       #~(begin
           (use-modules ((srfi srfi-64) #:select (test-begin test-end test-assert
                                                test-equal test-runner-current))
                        ((gnu build marionette)
                         #:select (make-marionette marionette-eval
                                   wait-for-tcp-port system-test-runner)))
           (define marionette (make-marionette (list #$vm "-cpu" "max")))
           (define (start-member)
             (marionette-eval
              '(system* #$(file-append bash-minimal "/bin/sh")
                        "-c"
                        (string-append
                         #$etcd "/bin/etcd"
                         " --name package-test --data-dir=/tmp/etcd-data"
                         " --listen-client-urls=http://127.0.0.1:2379"
                         " --advertise-client-urls=http://127.0.0.1:2379"
                         " --listen-peer-urls=http://127.0.0.1:2380"
                         " --initial-advertise-peer-urls=http://127.0.0.1:2380"
                         " --initial-cluster=package-test=http://127.0.0.1:2380"
                         " > /tmp/etcd.log 2>&1 & echo $! > /tmp/etcd.pid"))
              marionette))
           (define (stop-member)
             (marionette-eval
              '(let ((pid (call-with-input-file "/tmp/etcd.pid" read)))
                 (kill pid SIGTERM)
                 (let loop ((remaining 100))
                   (if (not (file-exists? (format #f "/proc/~a" pid)))
                       #t
                       (and (> remaining 0)
                            (begin (usleep 100000) (loop (- remaining 1)))))))
              marionette))
           (define (read-value)
             (marionette-eval
              '(begin
                 (use-modules ((ice-9 popen) #:select (open-pipe* close-pipe))
                              ((ice-9 textual-ports) #:select (get-string-all)))
                 (let* ((pipe (open-pipe* OPEN_READ
                                          #$(file-append etcd "/bin/etcdctl")
                                          "--endpoints=http://127.0.0.1:2379"
                                          "get" "upgrade-test" "--print-value-only"))
                        (text (get-string-all pipe)))
                   (and (zero? (close-pipe pipe)) text)))
              marionette))
           (test-runner-current (system-test-runner #$output))
           (test-begin "etcd")
           (test-assert "start member" (zero? (start-member)))
           (test-assert "client port ready"
             (wait-for-tcp-port 2379 marionette #:timeout 60))
           (test-equal "write key" 0
             (marionette-eval
              '(system* #$(file-append etcd "/bin/etcdctl")
                        "--endpoints=http://127.0.0.1:2379"
                        "put" "upgrade-test" "persisted")
              marionette))
           (test-equal "read key" "persisted\n" (read-value))
           (test-assert "stop member" (stop-member))
           (test-assert "second start" (zero? (start-member)))
           (test-assert "client port ready after restart"
             (wait-for-tcp-port 2379 marionette #:timeout 60))
           (test-equal "key survives restart" "persisted\n" (read-value))
           (test-assert "final stop" (stop-member))
           (test-end)))))))
