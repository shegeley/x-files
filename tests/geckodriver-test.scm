(use-modules ((ares suitbl) #:select (current-test-runner define-suite get-state
                                    is make-suitbl test))
             ((ares suitbl state) #:select (get-run-history))
             ((guix packages) #:select (package package-version))
             ((nongnu packages mozilla) #:select (firefox))
             ((x-files packages geckodriver) #:select (geckodriver-for))
             ((srfi srfi-1) #:select (every)))

(define-suite (geckodriver-compatibility-tests)
  (test "geckodriver 0.37.1 rejects Firefox below 115 ESR" ()
    (is (catch #t
          (lambda ()
            (geckodriver-for (package (inherit firefox) (version "114.0")))
            #f)
          (lambda _ #t))))
  (test "the supported boundary and current browser are accepted" ()
    (is (string=? "0.37.1"
                  (package-version
                   (geckodriver-for (package (inherit firefox) (version "115.0esr"))))))
    (is (string=? "0.37.1" (package-version (geckodriver-for firefox))))))

(let ((runner (make-suitbl)))
  (parameterize ((current-test-runner runner)) (geckodriver-compatibility-tests))
  (exit (if (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome)))
                   (get-run-history (get-state runner))) 0 1)))
