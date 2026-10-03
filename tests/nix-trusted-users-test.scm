(use-modules ((ares suitbl) #:select (suite test is current-test-runner get-state make-suitbl))
             ((ares suitbl state) #:select (get-run-history))
             ((gnu services) #:select (service-kind service-type-name service-value))
             ((gnu services nix) #:select (nix-service-type))
             ((gnu system accounts) #:select (user-account user-group))
             ((guix gexp) #:select (plain-file-content))
             ((rde features) #:select (rde-config feature-system-services-getter
                                                  feature-home-services-getter))
             ((srfi srfi-1) #:select (find every))
             ((srfi srfi-13) #:select (string-contains string-suffix?))
             ((x-files features nix) #:select (feature-nix-dev)))

(define (daemon-config feature)
  (let ((service
         (find (lambda (service) (eq? (service-kind service) nix-service-type))
               ((feature-system-services-getter feature) (rde-config)))))
    (apply string-append
           ((@@ (gnu services nix) nix-configuration-extra-config)
            (service-value service)))))

(define (client-config feature)
  (let ((service
         (find (lambda (service)
                 (eq? (service-type-name (service-kind service)) 'nix-client-config))
               ((feature-home-services-getter feature) (rde-config)))))
    (plain-file-content (cadar (service-value service)))))

(define runner (make-suitbl))
(current-test-runner runner)
(suite "Nix trusted users"
       (test "Default preserves the daemon trust policy" ()
             (let ((feature (feature-nix-dev #:documentation? #f)))
               (is (not (string-contains (daemon-config feature) "trusted-users")))
               (is (not (string-contains (client-config feature) "trusted-users")))))
       (test "User and group entries configure only the daemon" ()
             (let ((feature (feature-nix-dev #:documentation? #f
                                             #:nix-trusted-users '("builder" "alice" "@builders"))))
               (is (string-suffix? "trusted-users = builder alice @builders\n"
                                   (daemon-config feature)))
               (is (not (string-contains (client-config feature) "trusted-users")))))
       (test "Explicit trust overrides legacy raw config even without a final newline" ()
             (let ((feature (feature-nix-dev
                             #:documentation? #f
                             #:extra-config '("max-jobs = 4\ntrusted-users = old")
                             #:nix-trusted-users '("builder" "alice"))))
               (is (string-contains (daemon-config feature) "max-jobs = 4\n"))
               (is (string-suffix? "\ntrusted-users = builder alice\n"
                                   (daemon-config feature)))))
       (test "Guix accounts and groups serialize alongside names" ()
             (let* ((account (user-account (name "alice") (group "users")))
                    (group (user-group (name "builders")))
                    (feature (feature-nix-dev
                              #:documentation? #f
                              #:nix-trusted-users (list account group "builder"))))
               (is (string-suffix? "trusted-users = alice @builders builder\n"
                                   (daemon-config feature)))
               (is (not (string-contains (client-config feature) "trusted-users")))))
       (test "Guix record names cannot inject Nix configuration" ()
             (for-each
              (lambda (entry)
                (is (catch #t
                      (lambda () (feature-nix-dev #:nix-trusted-users (list entry)) #f)
                      (lambda _ #t))))
              (list (user-account (name "alice\ntrusted-users = *") (group "users"))
                    (user-group (name "builders#comment"))
                    (user-group (name "")))))
       (test "Generic trust options remain daemon-only" ()
             (let ((feature (feature-nix-dev
                             #:documentation? #f
                             #:options '(("trusted-users" . ("builder" "alice"))
                                         ("extra-trusted-users" . ("@builders"))))))
               (is (string-contains (daemon-config feature) "trusted-users = builder alice\n"))
               (is (string-contains (daemon-config feature) "extra-trusted-users = @builders\n"))
               (is (not (string-contains (client-config feature) "trusted-users")))))
       (test "An explicit empty list is distinct from the default" ()
             (is (string-suffix? "trusted-users = \n"
                                 (daemon-config (feature-nix-dev #:nix-trusted-users '())))))
       (test "Reject malformed lists and configuration injection" ()
             (for-each
              (lambda (value)
                (is (catch #t
                      (lambda () (feature-nix-dev #:nix-trusted-users value) #f)
                      (lambda _ #t))))
              '("alice" (42) ("") ("alice bob") ("alice\ntrusted-users = *") ("alice#comment")))))

(let ((history (get-run-history (get-state runner))))
  (exit (if (and (pair? history)
                 (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome))) history))
            0 1)))
