(use-modules ((ares suitbl) #:select (current-test-runner define-suite get-state
                                    is make-suitbl test))
             ((ares suitbl state) #:select (get-run-history))
             ((guix build utils) #:select (delete-file-recursively mkdir-p))
             ((guix build syscalls) #:select (mkdtemp!))
             ((ice-9 ftw) #:select (scandir))
             ((ice-9 match) #:select (match))
             ((ice-9 popen) #:select (open-pipe* close-pipe))
             ((ice-9 ports) #:select (OPEN_READ))
             ((ice-9 textual-ports) #:select (get-string-all))
             ((srfi srfi-1) #:select (every first))
             ((sxml simple) #:select (sxml->xml)))

(define (read-file path)
  (call-with-input-file path get-string-all))

(define (write-file path text)
  (call-with-output-file path (lambda (port) (display text port))))

(define (exercise artifacts scenario directory)
  "Exercise @var{scenario} using the built guard in @var{artifacts}.
Activate a fixture @code{org.gnome.Shell} owner, then verify exit status,
restart counts, and idempotence on the private bus under @var{directory}.
Clear Guile search paths to check the guard's complete store closure."
  (setenv "XDG_CURRENT_DESKTOP" "GNOME")
  (unsetenv "GUILE_LOAD_PATH")
  (unsetenv "GUILE_LOAD_COMPILED_PATH")
  (let ((busctl (string-append artifacts "/busctl"))
        (guard (string-append artifacts "/guard")))
    (define (invoke . arguments)
      (let* ((port (apply open-pipe* OPEN_READ guard arguments))
             (output (get-string-all port))
             (status (close-pipe port)))
        (format #t "~a" output)
        (zero? status)))
    (define (counts)
      (map (lambda (role)
             (let ((file (string-append directory "/" role)))
               (if (file-exists? file) (call-with-input-file file read) 0)))
           '("backend" "portal")))
    (unless (zero? (system* busctl "--user" "call" "org.freedesktop.DBus"
                           "/org/freedesktop/DBus" "org.freedesktop.DBus"
                           "StartServiceByName" "su" "org.gnome.Shell" "0"))
      (error "Could not activate fixture shell"))
    (when (equal? scenario "frontend")
      (is (not (invoke "--check")))
      (is (equal? '(1 1) (counts))))
    (let ((failure? (member scenario '("backend-failure" "frontend-failure")))
          (expected (assoc-ref '(("healthy" 1 1) ("frontend" 1 2) ("both" 2 2)
                                 ("backend-failure" 2 0) ("frontend-failure" 1 2))
                               scenario)))
      (is (eq? (not failure?) (invoke)))
      (is (equal? expected (counts)))
      (unless failure?
        (is (invoke))
        (is (invoke "--check")))
      (is (equal? expected (counts))))))

(define (run-private-bus artifacts fixture library root scenario)
  "Run @var{scenario} with isolated D-Bus activation files under @var{root}.
Expose only the Shell and portal services supplied by @var{fixture}, using
@var{library} for GIO and the executables from @var{artifacts}."
  (let* ((directory (string-append root "/" scenario))
         (services (string-append directory "/services"))
         (config (string-append directory "/bus.conf")))
    (mkdir-p services)
    (for-each
     (lambda (role name)
       (write-file
        (string-append services "/" name ".service")
        (format #f "[D-BUS Service]~%Name=~a~%Exec=~a --no-auto-compile ~a ~a ~a ~a ~a~%"
                name (string-append artifacts "/guile") fixture library
                role scenario directory)))
     '("shell" "backend" "portal")
     '("org.gnome.Shell" "org.freedesktop.impl.portal.desktop.gnome"
       "org.freedesktop.portal.Desktop"))
    (call-with-output-file config
      (lambda (port)
        (sxml->xml
         `(busconfig (type "session")
                     (listen ,(string-append "unix:path=" directory "/bus"))
                     (auth "EXTERNAL") (servicedir ,services)
                     (policy (@ (context "default"))
                             (allow (@ (own "*")))
                             (allow (@ (send_destination "*")))
                             (allow (@ (receive_sender "*"))))) port)))
    (zero? (system* (string-append artifacts "/dbus-run-session")
                    (string-append "--config-file=" config) "--"
                    "guix" "repl" "--" (first (command-line))
                    "exercise" artifacts scenario directory))))

(define (check-activation artifacts root)
  "Check the home activation in @var{artifacts} with a temporary home.
Exercise installation, repeated activation, replacement of a read-only
entry from an earlier generation, and cleanup after a failed replacement."
  (let* ((home (string-append root "/home"))
         (target (string-append home "/.config/autostart/gnome-portal-backend-guard.desktop")))
    (define (activate!)
      (zero? (system* "env" "-u" "GUILE_LOAD_PATH" "-u" "GUILE_LOAD_COMPILED_PATH"
                      (string-append "HOME=" home)
                      (string-append artifacts "/activate"))))
    (mkdir-p home)
    (for-each
     (lambda (previous)
       (when previous
         (when (file-exists? target) (delete-file target))
         (write-file target previous)
         (chmod target #o444))
       (is (activate!))
       (is (equal? (read-file target)
                   (read-file (string-append artifacts "/autostart.desktop")))))
     '(#f #f "previous generation" #f))
    (delete-file target)
    (mkdir target)
    (is (activate!))
    (is (eq? 'directory (stat:type (stat target))))
    (is (equal? '("." ".." "gnome-portal-backend-guard.desktop")
                (scandir (dirname target))))))

(let ((runner (make-suitbl)))
  (parameterize ((current-test-runner runner))
    (match (command-line)
      ((_ "exercise" artifacts scenario directory)
       (define-suite (private-bus-test)
         (test "built guard and real busctl/kill" ()
           (exercise artifacts scenario directory)))
       (private-bus-test))
      ((_ artifacts fixture library)
       (let ((root (mkdtemp! "/tmp/portal-guard-test-XXXXXX")))
         (dynamic-wind
           (lambda () #t)
           (lambda ()
             (define-suite (portal-guard-integration-tests)
               (test "activation is repeatable across generation switches" ()
                 (check-activation artifacts root))
               (test "private D-Bus recovery scenarios" ()
                 (for-each
                  (lambda (scenario)
                    (is (run-private-bus artifacts fixture library root scenario)))
                  '("healthy" "frontend" "both" "backend-failure" "frontend-failure"))))
             (portal-guard-integration-tests))
           (lambda () (delete-file-recursively root)))))))
  (let ((history (get-run-history (get-state runner))))
    (exit (if (and (pair? history)
                   (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome)))
                          history)) 0 1))))
