(use-modules ((x-files features desktop portal-guard)
              #:select (repair-screen-cast-portal!))
             ((ares suitbl) #:select (current-test-runner define-suite get-state
                                    is make-suitbl test))
             ((ares suitbl state) #:select (get-run-history))
             ((srfi srfi-1) #:select (every)))

(define* (fixture #:key
                  (shell? #t)
                  (backend-types 7)
                  (portal-types 7)
                  (backend-recovers? #t)
                  (portal-recovers? #t))
  "Return an alist containing a repair runner and restart counters.
@var{backend-recovers?} and @var{portal-recovers?} control whether the
corresponding repair changes the fixture's source mask to @code{7}."
  (let ((backend backend-types)
        (portal portal-types)
        (backend-restarts 0)
        (portal-restarts 0))
    (define (run!)
      (repair-screen-cast-portal!
       #:shell-ready? (lambda () shell?)
       #:backend-source-types (lambda () backend)
       #:portal-source-types (lambda () portal)
       #:restart-backend!
       (lambda ()
         (set! backend-restarts (1+ backend-restarts))
         (when backend-recovers?
           (set! backend 7))
         backend-recovers?)
       #:restart-portal!
       (lambda ()
         (set! portal-restarts (1+ portal-restarts))
         (when portal-recovers?
           (set! portal 7))
         portal-recovers?)
       #:pause (lambda (_) #t)
       #:shell-attempts 2
       #:backend-attempts 2
       #:portal-attempts 2))
    `((run! . ,run!)
      (backend-restarts . ,(lambda () backend-restarts))
      (portal-restarts . ,(lambda () portal-restarts)))))

(define (fixture-call state key)
  ((assoc-ref state key)))

(define-suite (portal-guard-tests)
  (test "healthy portals need no action" ()
    (let* ((state (fixture))
           (result (fixture-call state 'run!)))
      (is (eq? 'healthy (assoc-ref result 'status)))
      (is (null? (assoc-ref result 'actions)))))

  (test "stale frontend restarts only the frontend" ()
    (let* ((state (fixture #:portal-types 0))
           (result (fixture-call state 'run!)))
      (is (eq? 'repaired (assoc-ref result 'status)))
      (is (equal? '(portal) (assoc-ref result 'actions)))
      (is (= 1 (fixture-call state 'portal-restarts)))))

  (test "backend is repaired before the frontend" ()
    (let* ((state (fixture #:backend-types 0 #:portal-types 0))
           (result (fixture-call state 'run!)))
      (is (eq? 'repaired (assoc-ref result 'status)))
      (is (equal? '(backend portal) (assoc-ref result 'actions)))))

  (test "backend failure blocks frontend repair" ()
    (let* ((state (fixture #:backend-types 0 #:portal-types 0
                           #:backend-recovers? #f))
           (result (fixture-call state 'run!)))
      (is (eq? 'unavailable (assoc-ref result 'status)))
      (is (eq? 'backend (assoc-ref result 'failed-step)))
      (is (zero? (fixture-call state 'portal-restarts)))))

  (test "missing shell prevents all repair actions" ()
    (let* ((state (fixture #:shell? #f #:backend-types 0 #:portal-types 0))
           (result (fixture-call state 'run!)))
      (is (eq? 'unavailable (assoc-ref result 'status)))
      (is (eq? 'shell (assoc-ref result 'failed-step)))
      (is (null? (assoc-ref result 'actions)))))

  (test "frontend failure retains backend observations" ()
    (let* ((state (fixture #:portal-types 0 #:portal-recovers? #f))
           (result (fixture-call state 'run!)))
      (is (equal? '((status . unavailable)
                    (failed-step . portal)
                    (observations . ((shell . #t) (backend . 7) (portal . 0)))
                    (actions . (portal)))
                  result))))

  (test "missing backend does not require restarting a healthy frontend" ()
    (let* ((state (fixture #:backend-types #f))
           (result (fixture-call state 'run!)))
      (is (equal? '((status . repaired)
                    (failed-step . #f)
                    (observations . ((shell . #t) (backend . 7) (portal . 7)))
                    (actions . (backend)))
                  result))))

  (test "second repair is idempotent" ()
    (let* ((state (fixture #:backend-types 0 #:portal-types 0))
           (initial (fixture-call state 'run!))
           (repeated (fixture-call state 'run!)))
      (is (eq? 'repaired (assoc-ref initial 'status)))
      (is (eq? 'healthy (assoc-ref repeated 'status)))
      (is (= 1 (fixture-call state 'backend-restarts)))
      (is (= 1 (fixture-call state 'portal-restarts))))))

(let ((runner (make-suitbl)))
  (parameterize ((current-test-runner runner))
    (portal-guard-tests))
  (let ((history (get-run-history (get-state runner))))
    (exit (if (and (pair? history)
                   (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome)))
                          history)) 0 1))))
