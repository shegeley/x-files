(define-module (x-files services browser-history-manager)
  #:use-module ((gnu services) #:select (service-type
                                         service-extension))
  #:use-module ((gnu services shepherd) #:select (shepherd-service
                                                  shepherd-action
                                                  shepherd-trigger-action))
  #:use-module ((gnu home services shepherd) #:select (home-shepherd-service-type))
  #:use-module ((shepherd support) #:select (%user-log-dir))
  #:use-module ((x-files storage-files) #:select (browser-history-manager-scrub))
  #:use-module (guix gexp)
  #:use-module ((gnu packages base) #:select (grep))
  #:use-module ((gnu packages guile) #:select (guile-sqlite3))
  #:use-module ((gnu packages guile-xyz) #:select (guile-fibers))
  #:use-module ((x-files packages guile-fsnotify) #:select (guile-fsnotify))

  #:export (browser-history-manager-config
            home-browser-history-manager-timer-service-type
            home-browser-history-manager-inotify-service-type))

;;; Two independent home-shepherd service-types that scrub matching browser
;;; history/traces, sharing the pure logic in
;;; resources/browser-history-manager/browser-history-manager.scm.  Both drive
;;; the SAME `scrub!' entry point: the timer runs it on a calendar schedule, the
;;; watcher on debounced inotify events (native guile-fsnotify + guile-fibers).

(define %storage-module
  ;; The (browser-history-manager) logic module.  Kept under the channel's
  ;; storage/ dir — OFF the src/ module path — so `guix pull' never tries to
  ;; compile its (sqlite3) import (guile-sqlite3 is not on the channel build
  ;; load path).  Resolved cuirass-safely via (x-files storage-files).
  browser-history-manager-scrub)

;;;
;;; Config (plain alist, no records)
;;;

(define* (browser-history-manager-config
          #:key
          (patterns '())
          (aspects '(history autocomplete cookies downloads))
          (extra-backends '())
          (dry-run? #f)
          (touch-bookmarks? #f)
          (force-locked? #f)
          (interval-minutes 15)
          (debounce-seconds 20)
          (log-file (string-append %user-log-dir "/browser-history-manager.log"))
          (watch-log-file (string-append %user-log-dir
                                         "/browser-history-manager-watch.log")))
  `((patterns         . ,patterns)
    (aspects          . ,aspects)
    (extra-backends   . ,extra-backends)
    (dry-run?         . ,dry-run?)
    (touch-bookmarks? . ,touch-bookmarks?)
    (force-locked?    . ,force-locked?)
    (interval-minutes . ,interval-minutes)
    (debounce-seconds . ,debounce-seconds)
    (log-file         . ,log-file)
    (watch-log-file   . ,watch-log-file)))

(define %default-config (browser-history-manager-config))

;;;
;;; Programs
;;;

(define (minute-marks interval)
  "Calendar minute marks for a cadence of INTERVAL minutes (must divide 60)."
  (let loop ((m 0) (acc '()))
    (if (< m 60)
        (loop (+ m interval) (cons m acc))
        (reverse acc))))

(define (scrub-program config)
  "A program that runs one full scrub pass, per CONFIG."
  (let ((patterns         (assq-ref config 'patterns))
        (aspects          (assq-ref config 'aspects))
        (extra-backends   (assq-ref config 'extra-backends))
        (dry-run?         (assq-ref config 'dry-run?))
        (touch-bookmarks? (assq-ref config 'touch-bookmarks?))
        (force-locked?    (assq-ref config 'force-locked?)))
    (program-file
     "browser-history-manager-scrub"
     (with-extensions (list guile-sqlite3)
       (with-imported-modules
         `(((browser-history-manager) => ,%storage-module))
         #~(begin
             (use-modules ((browser-history-manager) #:select (scrub!)))
             (scrub! #:patterns '#$patterns
                     #:aspects '#$aspects
                     #:extra-backends '#$extra-backends
                     #:dry-run? #$dry-run?
                     #:touch-bookmarks? #$touch-bookmarks?
                     #:force-locked? #$force-locked?
                     #:grep #$(file-append grep "/bin/grep"))))))))

(define (watcher-program config)
  "A long-running program that scrubs on debounced inotify events, per CONFIG."
  (let ((patterns         (assq-ref config 'patterns))
        (aspects          (assq-ref config 'aspects))
        (extra-backends   (assq-ref config 'extra-backends))
        (dry-run?         (assq-ref config 'dry-run?))
        (touch-bookmarks? (assq-ref config 'touch-bookmarks?))
        (force-locked?    (assq-ref config 'force-locked?))
        (debounce         (assq-ref config 'debounce-seconds)))
    (program-file
     "browser-history-manager-watch"
     (with-extensions (list guile-fsnotify guile-fibers guile-sqlite3)
       (with-imported-modules
         `(((browser-history-manager) => ,%storage-module))
         #~(begin
             (use-modules
              ((linux inotify) #:select (make-inotify
                                         inotify-add-watch!
                                         inotify-read-event))
              ((fibers) #:select (run-fibers spawn-fiber))
              ((fibers channels) #:select (make-channel
                                           put-message
                                           get-message
                                           get-operation))
              ((fibers operations) #:select (perform-operation
                                             choice-operation
                                             wrap-operation))
              ((fibers timers) #:select (sleep-operation))
              ((browser-history-manager) #:select (scrub! dirs-to-watch)))

             (define (do-scrub)
               (scrub! #:patterns '#$patterns
                       #:aspects '#$aspects
                       #:extra-backends '#$extra-backends
                       #:dry-run? #$dry-run?
                       #:touch-bookmarks? #$touch-bookmarks?
                       #:force-locked? #$force-locked?
                       #:grep #$(file-append grep "/bin/grep")))

             ;; Block for the first event, keep swallowing events until a full
             ;; DEBOUNCE-second gap, then scrub once — coalescing WAL bursts.
             (define (settle! ch)
               (let loop ()
                 (when (eq? 'more
                            (perform-operation
                             (choice-operation
                              (wrap-operation (get-operation ch)
                                              (lambda (_) 'more))
                              (wrap-operation (sleep-operation #$debounce)
                                              (lambda (_) 'idle)))))
                   (loop))))

             (run-fibers
              (lambda ()
                (let ((ch   (make-channel))
                      (dirs (dirs-to-watch)))
                  (for-each
                   (lambda (dir)
                     (spawn-fiber
                      (lambda ()
                        (let ((ino (make-inotify)))
                          (inotify-add-watch! ino dir
                                              '(close-write moved-to create))
                          (let loop ()
                            (put-message ch (inotify-read-event ino))
                            (loop))))))
                   dirs)
                  (let loop ()
                    (get-message ch)          ; wait for a first event
                    (settle! ch)              ; coalesce the burst
                    (do-scrub)
                    (loop))))
              #:drain? #t)))))))

;;;
;;; Shepherd services
;;;

(define (timer-shepherd-service config)
  (shepherd-service
   (provision '(browser-history-manager-timer))
   (modules '((shepherd service timer)))
   (start #~(make-timer-constructor
             (calendar-event
              #:minutes '#$(minute-marks (assq-ref config 'interval-minutes)))
             (command (list #$(scrub-program config)))
             #:log-file #$(assq-ref config 'log-file)
             #:wait-for-termination? #t))
   (stop #~(make-timer-destructor))
   (documentation "Periodically scrub matching browser history entries.")
   (actions (list (shepherd-action
                    (inherit shepherd-trigger-action)
                    (documentation "Run a browser-history scrub now."))))))

(define (inotify-shepherd-service config)
  (shepherd-service
   (provision '(browser-history-manager-watcher))
   (start #~(make-forkexec-constructor
             (list #$(watcher-program config))
             #:log-file #$(assq-ref config 'watch-log-file)))
   (stop #~(make-kill-destructor))
   (documentation
    "Watch browser profiles and scrub matching history in near-real-time.")))

;;;
;;; Service types
;;;

(define home-browser-history-manager-timer-service-type
  (service-type
   (name 'home-browser-history-manager-timer)
   (default-value %default-config)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           (lambda (config) (list (timer-shepherd-service config))))))
   (description "Periodic browser-history scrubber (shepherd timer).")))

(define home-browser-history-manager-inotify-service-type
  (service-type
   (name 'home-browser-history-manager-inotify)
   (default-value %default-config)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           (lambda (config) (list (inotify-shepherd-service config))))))
   (description "Real-time browser-history scrubber (inotify watcher).")))
