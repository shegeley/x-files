(define-module (x-files tests utils recovery)
  #:use-module ((ares suitbl) #:select (define-suite is test throws-exception?))
  #:use-module ((x-files utils recovery) #:select (recover-in-order!)))

(define* (step name probe #:key
               (healthy? (lambda (value) value))
               repair!
               (attempts 1))
  `((name . ,name)
    (probe . ,probe)
    (healthy? . ,healthy?)
    (repair! . ,repair!)
    (attempts . ,attempts)))

(define (unexpected-call . arguments)
  (error "Unexpected callback" arguments))

(define-suite (recovery-tests)
  (test "empty plan is healthy" ()
    (is (equal? '((status . healthy)
                  (failed-step . #f)
                  (observations . ())
                  (actions . ()))
                (recover-in-order! '() #:pause unexpected-call))))

  (test "the predicate can accept false and zero values" ()
    (let ((result
           (recover-in-order!
            (list (step 'absent (lambda () #f)
                        #:healthy? not #:repair! unexpected-call)
                  (step 'empty (lambda () 0)
                        #:healthy? zero? #:repair! unexpected-call))
            #:pause unexpected-call)))
      (is (eq? 'healthy (assq-ref result 'status)))
      (is (equal? '((absent . #f) (empty . 0))
                  (assq-ref result 'observations)))
      (is (null? (assq-ref result 'actions)))))

  (test "wait for natural recovery before attempting repair" ()
    (let* ((calls 0)
           (pauses '())
           (result
            (recover-in-order!
             (list (step 'database
                         (lambda ()
                           (set! calls (1+ calls))
                           (if (= calls 3) 'ready #f))
                         #:repair! unexpected-call #:attempts 3))
             #:delay 2
             #:pause (lambda (delay) (set! pauses (cons delay pauses))))))
      (is (= 3 calls))
      (is (equal? '(2 2) pauses))
      (is (eq? 'healthy (assq-ref result 'status)))
      (is (equal? '((database . ready)) (assq-ref result 'observations)))))

  (test "repair dependencies before probing dependents; reruns are harmless" ()
    (let* ((database? #f)
           (application? #f)
           (events '())
           (record! (lambda (event) (set! events (cons event events)))))
      (let* ((steps
              (list
               (step 'database
                     (lambda () (record! 'probe-database) database?)
                     #:repair! (lambda ()
                                 (record! 'repair-database)
                                 (set! database? #t)))
               (step 'application
                     (lambda () (record! 'probe-application) application?)
                     #:repair! (lambda ()
                                 (record! 'repair-application)
                                 (set! application? #t)))))
             (result (recover-in-order! steps #:pause unexpected-call)))
        (is (eq? 'repaired (assq-ref result 'status)))
        (is (equal? '(database application) (assq-ref result 'actions)))
        (is (equal? '(probe-database repair-database probe-database
                      probe-application repair-application probe-application)
                    (reverse events)))
        (set! events '())
        (is (eq? 'healthy
                 (assq-ref (recover-in-order! steps #:pause unexpected-call)
                           'status)))
        (is (equal? '(probe-database probe-application) (reverse events))))))

  (test "an unavailable prerequisite blocks every dependent callback" ()
    (let ((result
           (recover-in-order!
            (list (step 'socket (lambda () 'missing)
                        #:healthy? (lambda (value) (eq? value 'ready)))
                  (step 'database unexpected-call #:repair! unexpected-call))
            #:pause unexpected-call)))
      (is (eq? 'unavailable (assq-ref result 'status)))
      (is (eq? 'socket (assq-ref result 'failed-step)))
      (is (equal? '((socket . missing)) (assq-ref result 'observations)))
      (is (null? (assq-ref result 'actions)))))

  (test "failed recovery is bounded and reports the last observation" ()
    (let* ((probes 0)
           (repairs 0)
           (pauses 0)
           (result
            (recover-in-order!
             (list (step 'database
                         (lambda () (set! probes (1+ probes)) 'starting)
                         #:healthy? (lambda (value) (eq? value 'ready))
                         #:repair! (lambda () (set! repairs (1+ repairs)) #f)
                         #:attempts 2)
                   (step 'application unexpected-call #:repair! unexpected-call))
             #:pause (lambda (_) (set! pauses (1+ pauses))))))
      (is (= 4 probes))
      (is (= 1 repairs))
      (is (= 2 pauses))
      (is (eq? 'unavailable (assq-ref result 'status)))
      (is (eq? 'database (assq-ref result 'failed-step)))
      (is (equal? '((database . starting)) (assq-ref result 'observations)))
      (is (equal? '(database) (assq-ref result 'actions)))))

  (test "reprobe even when repair returns false after changing state" ()
    (let* ((ready? #f)
           (result
            (recover-in-order!
             (list (step 'database (lambda () ready?)
                         #:repair! (lambda () (set! ready? #t) #f)))
             #:pause unexpected-call)))
      (is (eq? 'repaired (assq-ref result 'status)))
      (is (equal? '((database . #t)) (assq-ref result 'observations)))))

  (test "callback exceptions propagate without retry or downstream work" ()
    (let ((repairs 0))
      (is (eq? 'repair-error
               (catch 'repair-error
                 (lambda ()
                   (recover-in-order!
                    (list (step 'database (lambda () #f)
                                #:repair! (lambda ()
                                            (set! repairs (1+ repairs))
                                            (throw 'repair-error)))
                          (step 'application unexpected-call))
                    #:pause unexpected-call))
                 (lambda _ 'repair-error))))
      (is (= 1 repairs)))
    (is (eq? 'probe-error
             (catch 'probe-error
               (lambda ()
                 (recover-in-order!
                  (list (step 'database (lambda () (throw 'probe-error))
                              #:repair! unexpected-call))
                  #:pause unexpected-call))
               (lambda _ 'probe-error)))))

  (test "global attempts apply unless a step overrides them" ()
    (let* ((probes 0)
           (result
            (recover-in-order!
             `(((name . database)
                (probe . ,(lambda () (set! probes (1+ probes)) (= probes 2)))
                (healthy? . ,(lambda (value) value))))
             #:attempts 2 #:pause (lambda (_) #t))))
      (is (= 2 probes))
      (is (eq? 'healthy (assq-ref result 'status)))))

  (test "reject invalid later steps before any earlier side effects" ()
    (let ((probes 0))
      (for-each
       (lambda (invalid)
         (is (throws-exception?
              (recover-in-order!
               (list (step 'database (lambda () (set! probes (1+ probes)) #f)
                           #:repair! unexpected-call)
                     invalid)
               #:pause unexpected-call))))
       (list (step 'application (lambda () #t) #:attempts 0)
             (step 'application (lambda () #t) #:attempts -1)
             (step 'application (lambda () #t) #:attempts 1.5)
             (step 'application #f)
             (step 'application (lambda () #t) #:healthy? #f)
             (step 'application (lambda () #t) #:repair! 'restart)
             (step 'database (lambda () #t))))
      (is (zero? probes)))))
