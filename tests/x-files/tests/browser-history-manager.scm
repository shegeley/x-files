(define-module (x-files tests browser-history-manager)
  #:use-module ((gnu tests) #:select (system-test
                                      marionette-operating-system
                                      simple-operating-system))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((x-files storage-files) #:select (browser-history-manager-scrub))
  #:use-module ((gnu packages base) #:select (grep))
  #:use-module ((gnu packages guile) #:select (guile-sqlite3))
  #:use-module (guix gexp)

  #:export (%test-browser-history-manager))

;;; Marionette VM test for the pure guile-sqlite3 scrub logic.  No real browser
;;; is needed: we seed a Firefox `places.sqlite' and a Chromium `History' with
;;; known rows, run `scrub!', and assert what remains.  Covers the happy path,
;;; the bookmark guard (a bookmarked matching page's PLACE row survives while
;;; its visits are cleared), and dry-run (nothing deleted).

(define %storage-module
  ;; Off-module-path logic file (see the service module).
  browser-history-manager-scrub)

(define %ff-db "/root/.mozilla/firefox/test.default/places.sqlite")
(define %cr-db "/root/.config/chromium/Default/History")

(define seed-program
  ;; Create both DBs with fixtures: a matching unbookmarked page, a matching
  ;; BOOKMARKED page, and a non-matching control — in each browser family.
  (program-file
   "bhm-seed"
   (with-extensions (list guile-sqlite3)
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        ((sqlite3) #:select (sqlite-open sqlite-exec sqlite-close)))
           (mkdir-p (dirname #$%ff-db))
           (mkdir-p (dirname #$%cr-db))
           (let ((db (sqlite-open #$%ff-db)))
             (sqlite-exec db "\
CREATE TABLE moz_places (id INTEGER PRIMARY KEY, url TEXT, foreign_count INTEGER DEFAULT 0);
CREATE TABLE moz_historyvisits (id INTEGER PRIMARY KEY, place_id INTEGER);
CREATE TABLE moz_bookmarks (id INTEGER PRIMARY KEY, fk INTEGER);
INSERT INTO moz_places (id,url,foreign_count) VALUES
  (1,'https://nsa.gov/x',0),(2,'https://nsa.gov/saved',1),(3,'https://ok.example/',0);
INSERT INTO moz_historyvisits (id,place_id) VALUES (1,1),(2,2),(3,3);
INSERT INTO moz_bookmarks (id,fk) VALUES (1,2);")
             (sqlite-close db))
           (let ((db (sqlite-open #$%cr-db)))
             (sqlite-exec db "\
CREATE TABLE urls (id INTEGER PRIMARY KEY, url TEXT);
CREATE TABLE visits (id INTEGER PRIMARY KEY, url INTEGER);
INSERT INTO urls (id,url) VALUES (1,'https://nsa.gov/x'),(2,'https://ok.example/');
INSERT INTO visits (id,url) VALUES (1,1),(2,2);")
             (sqlite-close db))
           (exit 0))))))

(define (scrub-program dry-run?)
  (program-file
   (string-append "bhm-scrub-" (if dry-run? "dry" "real"))
   (with-extensions (list guile-sqlite3)
     (with-imported-modules
       `(((browser-history-manager) => ,%storage-module))
       #~(begin
           (use-modules ((browser-history-manager) #:select (scrub!)))
           (scrub! #:patterns '("nsa.gov")
                   #:aspects '(history)
                   #:home "/root"
                   #:dry-run? #$dry-run?
                   #:grep #$(file-append grep "/bin/grep"))
           (exit 0))))))

(define count-program
  ;; Write an alist of match counts to the file named on argv[1].
  (program-file
   "bhm-count"
   (with-extensions (list guile-sqlite3)
     #~(begin
         (use-modules ((sqlite3) #:select (sqlite-open sqlite-prepare sqlite-step
                                           sqlite-finalize sqlite-close
                                           SQLITE_OPEN_READONLY)))
         (define (count db-path sql)
           (catch #t
             (lambda ()
               (let* ((db   (sqlite-open db-path SQLITE_OPEN_READONLY))
                      (stmt (sqlite-prepare db sql))
                      (row  (sqlite-step stmt))
                      (n    (if row (vector-ref row 0) 0)))
                 (sqlite-finalize stmt)
                 (sqlite-close db)
                 n))
             (lambda _ -1)))
         (call-with-output-file (cadr (command-line))
           (lambda (p)
             (write
              (list
               (cons 'places-nsa
                     (count #$%ff-db
                            "SELECT count(*) FROM moz_places WHERE url LIKE '%nsa.gov%'"))
               (cons 'visits-nsa
                     (count #$%ff-db
                            "SELECT count(*) FROM moz_historyvisits WHERE place_id IN (SELECT id FROM moz_places WHERE url LIKE '%nsa.gov%')"))
               (cons 'places-control
                     (count #$%ff-db
                            "SELECT count(*) FROM moz_places WHERE url LIKE '%ok.example%'"))
               (cons 'urls-nsa
                     (count #$%cr-db
                            "SELECT count(*) FROM urls WHERE url LIKE '%nsa.gov%'"))
               (cons 'urls-control
                     (count #$%cr-db
                            "SELECT count(*) FROM urls WHERE url LIKE '%ok.example%'")))
              p)))
         (exit 0)))))

(define %os
  (marionette-operating-system (simple-operating-system)))

(define (run-test name)
  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$(virtual-machine %os))))

          (define (run . argv)
            (marionette-eval
             `(zero? (status:exit-val (apply system* ',argv)))
             marionette))

          (define (read-counts file)
            (marionette-eval `(call-with-input-file ,file read) marionette))

          (test-runner-current (system-test-runner #$output))
          (test-begin "browser-history-manager")

          (test-assert "seed databases"
            (run #$seed-program))

          ;; Dry run must not delete anything.
          (test-assert "dry-run scrub" (run #$(scrub-program #t)))
          (test-assert "count after dry-run"
            (run #$count-program "/root/after-dry.scm"))
          (test-equal "dry-run deletes nothing"
            '((places-nsa . 2) (visits-nsa . 2) (places-control . 1)
              (urls-nsa . 1) (urls-control . 1))
            (read-counts "/root/after-dry.scm"))

          ;; Real run removes matches, keeps the bookmarked place and controls.
          (test-assert "real scrub" (run #$(scrub-program #f)))
          (test-assert "count after real"
            (run #$count-program "/root/after.scm"))
          (test-equal "removes matches, keeps bookmark + controls"
            '((places-nsa . 1) (visits-nsa . 0) (places-control . 1)
              (urls-nsa . 0) (urls-control . 1))
            (read-counts "/root/after.scm"))

          (test-end))))

  (gexp->derivation name test))

(define %test-browser-history-manager
  (system-test
   (name "browser-history-manager")
   (description
    "Seed Firefox/Chromium history DBs, run the pattern scrubber, and assert
matching history is removed while bookmarks and non-matching sites survive.")
   (value (run-test name))))
