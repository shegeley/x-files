(use-modules ((ares suitbl) #:select (current-test-runner get-state
                                    is make-suitbl suite test))
             ((ares suitbl state) #:select (get-run-history))
             ((guix build syscalls) #:select (mkdtemp!))
             ((guix build utils) #:select (delete-file-recursively substitute*
                                          which))
             ((guix utils) #:select (with-environment-variables))
             ((ice-9 popen) #:select (close-pipe open-pipe*))
             ((ice-9 ports) #:select (OPEN_READ))
             ((ice-9 textual-ports) #:select (get-string-all))
             ((srfi srfi-1) #:select (every))
             ((srfi srfi-13) #:select (string-trim-right)))

(define %gl-probe
  "(dynamic-func \"glXGetProcAddress\" (dynamic-link \"libGLX.so.0\"))
   (dynamic-func \"glGetString\" (dynamic-link \"libOpenGL.so.0\"))")

(define (binary-rpath binary)
  "Read the installed search path of @var{binary}."
  (let* ((pipe (open-pipe* OPEN_READ "patchelf" "--print-rpath" binary))
         (path (string-trim-right (get-string-all pipe)))
         (status (close-pipe pipe)))
    (unless (zero? status) (error "Cannot read RPATH" binary))
    path))

(define (max-loader-tests package directory)
  (suite (string-append "MAX loader: " package)
    (test "launcher resolves GLX and OpenGL entry points" ()
      ;; Keep the installed launcher's environment, replacing only its final
      ;; executable with Guile.  Never start MAX or touch a messenger profile.
      (let ((launcher (string-append directory "/max")))
        (copy-file (string-append package "/bin/max") launcher)
        (chmod launcher #o700)
        (substitute* launcher
          (((string-append package "/share/max/bin/max")) (which "guile")))
        (with-environment-variables '(("DISPLAY" ":0") ("XAUTHORITY" "/dev/null")
                                      ("LD_LIBRARY_PATH" #f) ("LD_BIND_NOW" "1"))
          (is (zero? (system* launcher "--no-auto-compile" "-c" %gl-probe))))))
    (test "all patched executables resolve GLX and OpenGL through RPATH" ()
      (for-each
       (lambda (relative)
         (format #t "Checking ~a/~a~%" package relative)
         (with-environment-variables
             `(("LD_LIBRARY_PATH" ,(binary-rpath (string-append package "/" relative)))
               ("LD_BIND_NOW" "1"))
           (is (zero? (system* "guile" "--no-auto-compile" "-c" %gl-probe)))))
       '("share/max/bin/max"
         "share/max/bin/max-service/bin/max-service"
         "share/max/libexec/QtWebEngineProcess")))))

(let ((runner (make-suitbl))
      (directory (mkdtemp! "/tmp/max-loader-test-XXXXXX")))
  (dynamic-wind
    (lambda () #t)
    (lambda ()
      (parameterize ((current-test-runner runner))
        (for-each (lambda (package) (max-loader-tests package directory))
                  (cdr (command-line)))))
    (lambda () (delete-file-recursively directory)))
  (let ((history (get-run-history (get-state runner))))
    (exit (if (and (pair? history)
                   (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome)))
                          history)) 0 1))))
