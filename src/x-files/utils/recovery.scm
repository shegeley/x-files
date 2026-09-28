(define-module (x-files utils recovery)
  #:use-module ((srfi srfi-1) #:select (first second (cdr . rest)))
  #:export (recover-in-order!))

(define (positive-integer? value)
  (and (exact-integer? value) (positive? value)))

(define* (recover-in-order! steps #:key (attempts 3) (delay 1) (pause sleep))
  "Check @var{steps} in dependency order, repairing each at most once.

Each step is an alist with a unique symbol @code{name}, a zero-argument
@code{probe}, and a @code{healthy?} predicate accepting its result.  Optional
@code{repair!} is a zero-argument procedure; without it, only wait.  Optional
@code{attempts} overrides @var{attempts}, a positive probe count per waiting
phase.  @var{pause} receives @var{delay} between unsuccessful probes, never
after the last one.

After repair, probe again even if the repair returned @code{#f}: an unsuccessful
action may still have changed state.  Exceptions propagate without retrying.
Attempt counts do not bound time spent inside callbacks.

Stop at the first unhealthy step.  Return an alist with @code{status}
(@code{healthy}, @code{repaired}, or @code{unavailable}), @code{failed-step}
(its name or @code{#f}), @code{observations} (visited names and their last probe
values), and @code{actions} (names whose repairs were attempted, in order).
A healthy probe value may itself be @code{#f}.  Validate all steps before probing."
  (define (step-attempts step)
    (let ((entry (assq 'attempts step)))
      (if entry (rest entry) attempts)))

  (define (poll step)
    (let ((probe (assq-ref step 'probe))
          (healthy? (assq-ref step 'healthy?)))
      (let loop ((remaining (step-attempts step)))
        (let ((value (probe)))
          (cond
           ((healthy? value) (list #t value))
           ((= remaining 1) (list #f value))
           (else
            (pause delay)
            (loop (1- remaining))))))))

  (define (result failed-step observations actions)
    `((status . ,(cond (failed-step 'unavailable)
                      ((null? actions) 'healthy)
                      (else 'repaired)))
      (failed-step . ,failed-step)
      (observations . ,(reverse observations))
      (actions . ,(reverse actions))))

  (unless (and (list? steps)
               (positive-integer? attempts)
               (real? delay) (>= delay 0)
               (procedure? pause))
    (error "Invalid recovery options" steps attempts delay pause))
  (let validate ((remaining steps) (names '()))
    (unless (null? remaining)
      (let* ((step (first remaining))
             (name (assq-ref step 'name))
             (repair! (assq-ref step 'repair!)))
        (unless (and (symbol? name)
                     (not (memq name names))
                     (procedure? (assq-ref step 'probe))
                     (procedure? (assq-ref step 'healthy?))
                     (or (not repair!) (procedure? repair!))
                     (positive-integer? (step-attempts step)))
          (error "Invalid recovery step" step))
        (validate (rest remaining) (cons name names)))))

  (let loop ((remaining steps) (observations '()) (actions '()))
    (if (null? remaining)
        (result #f observations actions)
        (let* ((step (first remaining))
               (name (assq-ref step 'name))
               (repair! (assq-ref step 'repair!))
               (sample (poll step)))
          (when (and (not (first sample)) repair!)
            (set! actions (cons name actions))
            (repair!)
            (set! sample (poll step)))
          (let ((observations (acons name (second sample) observations)))
            (if (first sample)
                (loop (rest remaining) observations actions)
                (result name observations actions)))))))
