(define-module (x-files tests features python)
  #:use-module ((gnu tests)              #:select (simple-operating-system
                                                   marionette-operating-system
                                                   system-test))
  #:use-module ((gnu system vm)          #:select (virtual-machine))
  #:use-module ((gnu system)             #:select (operating-system
                                                   operating-system-packages))
  #:use-module ((gnu services)           #:select (service))
  #:use-module ((gnu services networking) #:select (dhcpcd-service-type))

  #:use-module ((gnu packages python)      #:select (python))
  #:use-module ((gnu packages python-xyz)  #:select (python-lsp-server
                                                      python-debugpy))
  #:use-module ((gnu packages tree-sitter) #:select (tree-sitter-python))

  #:use-module (guix gexp)

  #:export (%test-python))

(define %python-os
  (operating-system
    (inherit (simple-operating-system
              (service dhcpcd-service-type)))
    (packages (cons* python python-lsp-server python-debugpy tree-sitter-python
                     (operating-system-packages
                      (simple-operating-system))))))

(define (run-python-test)
  (define os
    (marionette-operating-system
     %python-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette)
                       (ice-9 popen)
                       (rnrs io ports))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "python")

          (test-assert "python3 runs and prints output"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen) (rnrs io ports))
                (let* ((p   (open-pipe* OPEN_READ "python3" "-c" "print(42)"))
                       (out (get-string-all p)))
                  (close-pipe p)
                  (string=? (string-trim-right out) "42")))
             marionette))

          (test-assert "pylsp binary is present"
            (marionette-eval
             '(file-exists? "/run/current-system/profile/bin/pylsp")
             marionette))

          (test-assert "pylsp --version exits successfully"
            (marionette-eval
             '(zero? (system "pylsp --version > /dev/null 2>&1"))
             marionette))

          (test-assert "debugpy is importable"
            (marionette-eval
             '(zero? (system "python3 -c 'import debugpy' > /dev/null 2>&1"))
             marionette))

          (test-assert "debugpy version is readable"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen) (rnrs io ports))
                (let* ((p   (open-pipe* OPEN_READ
                                        "python3" "-c"
                                        "import debugpy; print(debugpy.__version__)"))
                       (out (get-string-all p)))
                  (close-pipe p)
                  (not (string-null? (string-trim-right out)))))
             marionette))

          (test-assert "tree-sitter-python grammar is installed"
            (marionette-eval
             '(file-exists? "/run/current-system/profile/lib/tree-sitter/python.so")
             marionette))

          (test-end))))

  (gexp->derivation "python-test" test))

(define %test-python
  (system-test
   (name "python")
   (description "Test that python3, pylsp, debugpy, and tree-sitter-python
are installed and functional.")
   (value (run-python-test))))
