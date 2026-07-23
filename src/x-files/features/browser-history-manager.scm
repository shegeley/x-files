(define-module (x-files features browser-history-manager)
  #:use-module ((rde features) #:select (feature))
  #:use-module ((gnu services) #:select (service))
  #:use-module ((x-files services browser-history-manager)
                #:select (browser-history-manager-config
                          home-browser-history-manager-timer-service-type
                          home-browser-history-manager-inotify-service-type))

  #:export (feature-browser-history-manager))

(define* (feature-browser-history-manager
          #:key
          (patterns '())
          (aspects '(history autocomplete cookies downloads))
          (extra-backends '())
          (timer? #t)
          (interval-minutes 15)
          (inotify? #t)
          (debounce-seconds 20)
          (dry-run? #f)
          (touch-bookmarks? #f)
          (force-locked? #f))
  "Scrub browser history (and other per-domain traces) matching PATTERNS.

PATTERNS is a declarative list of substrings, e.g. '(\"nsa.gov\" \"kremlin.ru\").
An empty list is a safe no-op.  ASPECTS selects which traces to remove
(history autocomplete cookies downloads cache).  Two independent triggers, each
toggleable: a shepherd TIMER? every INTERVAL-MINUTES and an INOTIFY? watcher
(DEBOUNCE-SECONDS coalescing).  Safe by default: bookmarks are kept unless
TOUCH-BOOKMARKS?, locked/running DBs are skipped unless FORCE-LOCKED?, and
DRY-RUN? logs matches without deleting."

  (define config
    (browser-history-manager-config
     #:patterns patterns
     #:aspects aspects
     #:extra-backends extra-backends
     #:dry-run? dry-run?
     #:touch-bookmarks? touch-bookmarks?
     #:force-locked? force-locked?
     #:interval-minutes interval-minutes
     #:debounce-seconds debounce-seconds))

  (define (get-home-services _config)
    (append
     (if timer?
         (list (service home-browser-history-manager-timer-service-type config))
         '())
     (if inotify?
         (list (service home-browser-history-manager-inotify-service-type config))
         '())))

  (feature
   (name 'browser-history-manager)
   (values `((browser-history-manager . #t)))
   (home-services-getter get-home-services)))
