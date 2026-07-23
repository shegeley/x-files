(define-module (browser-history-manager)
  #:use-module ((srfi srfi-1) #:select (append-map filter-map every remove))
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module ((ice-9 format) #:select (format))
  #:use-module ((sqlite3) #:select (sqlite-open
                                    sqlite-close
                                    sqlite-exec
                                    sqlite-prepare
                                    sqlite-bind
                                    sqlite-step
                                    sqlite-finalize
                                    sqlite-busy-timeout
                                    SQLITE_OPEN_READWRITE))

  #:export (scrub!
            profile-dirs
            dirs-to-watch
            db-files-to-watch
            %browser-backends
            %firefox-backend
            %chromium-backend))

;;; browser-history-manager — pattern-based deletion of per-domain browser
;;; traces (history, autocomplete, cookies, downloads) plus a best-effort disk
;;; cache scan.  DB access is done through guile-sqlite3 with bound `?`
;;; parameters (no manual quote escaping).  Pure data + functions, no records.
;;;
;;; Limitations (by design):
;;;  - Per-domain disk cache is BEST-EFFORT only: caches are hash-keyed, not
;;;    queryable by domain, so we grep entry files for the literal domain and
;;;    delete matches.  Fragile and browser-version specific; off unless the
;;;    'cache aspect is requested.
;;;  - Chromium-family DBs are exclusively locked while the browser runs; such
;;;    DBs are logged and skipped unless #:force-locked? is #t.
;;;  - Firefox form-history is not domain-scoped (no host column), so there is
;;;    no 'forms aspect; likewise Chromium typed_count is not reset (only the
;;;    keyword_search_terms autocomplete rows are removed).

;;;
;;; Small utilities
;;;

(define (ts)
  (strftime "%Y-%m-%dT%H:%M:%SZ" (gmtime (current-time))))

(define (glob->regex s)
  "Translate a single glob path SEGMENT into an anchored regex string."
  (apply string-append
         (map (lambda (c)
                (case c
                  ((#\*) ".*")
                  ((#\?) ".")
                  ((#\. #\+ #\( #\) #\[ #\] #\{ #\} #\^ #\$ #\\ #\|) (string #\\ c))
                  (else (string c))))
              (string->list s))))

(define (glob-segments g)
  (remove string-null? (string-split g #\/)))

(define (expand-glob base segments)
  "Return the list of existing filesystem paths under BASE that match the
remaining glob SEGMENTS (a list of path components, each possibly containing
`*' / `?')."
  (match segments
    (() (if (file-exists? base) (list base) '()))
    ((seg . rest)
     (if (or (string-index seg #\*) (string-index seg #\?))
         (let ((rx (make-regexp (string-append "^" (glob->regex seg) "$")))
               (entries (or (scandir base) '())))
           (append-map
            (lambda (name)
              (if (and (not (member name '("." "..")))
                       (regexp-exec rx name))
                  (expand-glob (string-append base "/" name) rest)
                  '()))
            entries))
         (expand-glob (string-append base "/" seg) rest)))))

;;;
;;; Backend / aspect data model (alists, fully data-driven & overridable)
;;;
;;; An op describes one deletion unit against a single db file:
;;;   db             relative db filename inside the profile dir
;;;   table          primary table to delete matching rows from
;;;   match-col      column on TABLE to LIKE-match (when `via' is #f)
;;;   via            (jtable jid jmatch fkcol) — match TABLE rows whose FKCOL is
;;;                  in (SELECT JID FROM JTABLE WHERE JMATCH LIKE ?)
;;;   id-col         primary key of TABLE, used by `cascades'
;;;   cascades       list of (child-table . child-fk) deleted before the primary
;;;   bookmark-guard extra WHERE fragment kept unless #:touch-bookmarks? (Firefox)
;;;

(define %firefox-history-ops
  (list
   '((db . "places.sqlite")
     (table . "moz_places")
     (match-col . "url")
     (id-col . "id")
     (cascades . (("moz_historyvisits" . "place_id")
                  ("moz_inputhistory" . "place_id")
                  ("moz_annos" . "place_id")
                  ("moz_places_metadata" . "place_id")))
     (via . #f)
     (bookmark-guard . "foreign_count = 0 AND id NOT IN (SELECT fk FROM moz_bookmarks WHERE fk IS NOT NULL)"))))

(define %firefox-autocomplete-ops
  (list
   '((db . "places.sqlite")
     (table . "moz_inputhistory")
     (via . ("moz_places" "id" "url" "place_id"))
     (bookmark-guard . #f))))

(define %firefox-cookies-ops
  (list
   '((db . "cookies.sqlite")
     (table . "moz_cookies")
     (match-col . "host")
     (via . #f)
     (bookmark-guard . #f))))

(define %firefox-downloads-ops
  (list
   '((db . "places.sqlite")
     (table . "moz_annos")
     (via . ("moz_places" "id" "url" "place_id"))
     (bookmark-guard . #f))))

(define %firefox-backend
  `((family . firefox)
    (profile-globs . (".mozilla/firefox/*.default"
                      ".mozilla/firefox/*.default-*"
                      ".librewolf/*.default*"
                      ".floorp/*.default*"))
    (watch-db . "places.sqlite")
    (sql-aspects . ((history      . ,%firefox-history-ops)
                    (autocomplete . ,%firefox-autocomplete-ops)
                    (cookies      . ,%firefox-cookies-ops)
                    (downloads    . ,%firefox-downloads-ops)))
    (cache-globs . (".mozilla/firefox/*/cache2/entries"
                    ".cache/mozilla/firefox/*/cache2/entries"))))

(define %chromium-history-ops
  (list
   '((db . "History")
     (table . "urls")
     (match-col . "url")
     (id-col . "id")
     (cascades . (("visits" . "url")
                  ("keyword_search_terms" . "url_id")
                  ("segments" . "url_id")))
     (via . #f)
     (bookmark-guard . #f))))

(define %chromium-autocomplete-ops
  (list
   '((db . "History")
     (table . "keyword_search_terms")
     (via . ("urls" "id" "url" "url_id"))
     (bookmark-guard . #f))))

(define %chromium-cookies-ops
  (list
   '((db . "Cookies")
     (table . "cookies")
     (match-col . "host_key")
     (via . #f)
     (bookmark-guard . #f))))

(define %chromium-downloads-ops
  (list
   '((db . "History")
     (table . "downloads_url_chains")
     (match-col . "url")
     (via . #f)
     (bookmark-guard . #f))))

(define %chromium-backend
  `((family . chromium)
    (profile-globs . (".config/chromium/*"
                      ".config/google-chrome/*"
                      ".config/BraveSoftware/Brave-Browser/*"
                      ".config/vivaldi/*"))
    (watch-db . "History")
    (sql-aspects . ((history      . ,%chromium-history-ops)
                    (autocomplete . ,%chromium-autocomplete-ops)
                    (cookies      . ,%chromium-cookies-ops)
                    (downloads    . ,%chromium-downloads-ops)))
    (cache-globs . (".config/chromium/*/Cache/Cache_Data"
                    ".config/google-chrome/*/Cache/Cache_Data"))))

(define %browser-backends (list %firefox-backend %chromium-backend))

;;;
;;; SQL generation (one `?' parameter per statement, bound to "%pattern%")
;;;

(define (op-match-predicate op)
  "Boolean SQL expr with a single `?', evaluated against OP's primary table."
  (let ((via (assq-ref op 'via)))
    (if via
        (match via
          ((jtable jid jmatch fkcol)
           (format #f "~a IN (SELECT ~a FROM ~a WHERE ~a LIKE ?)"
                   fkcol jid jtable jmatch)))
        (format #f "~a LIKE ?" (assq-ref op 'match-col)))))

(define (op-where op touch-bookmarks?)
  (let ((pred  (op-match-predicate op))
        (guard (assq-ref op 'bookmark-guard)))
    (if (and guard (not touch-bookmarks?))
        (string-append pred " AND " guard)
        pred)))

(define (op-count-sql op touch-bookmarks?)
  (format #f "SELECT count(*) FROM ~a WHERE ~a"
          (assq-ref op 'table) (op-where op touch-bookmarks?)))

(define (op-primary-delete-sql op touch-bookmarks?)
  (format #f "DELETE FROM ~a WHERE ~a"
          (assq-ref op 'table) (op-where op touch-bookmarks?)))

(define (op-cascade-delete-sqls op)
  "Delete child rows of the pattern-matching primary rows (never guarded:
clearing a bookmarked page's visits does not touch the bookmark itself)."
  (let ((id-col (assq-ref op 'id-col))
        (table  (assq-ref op 'table))
        (pred   (op-match-predicate op)))
    (if id-col
        (map (match-lambda
               ((child . fk)
                (format #f "DELETE FROM ~a WHERE ~a IN (SELECT ~a FROM ~a WHERE ~a)"
                        child fk id-col table pred)))
             (or (assq-ref op 'cascades) '()))
        '())))

;;;
;;; SQLite execution
;;;

(define (missing-table? errmsg)
  (and errmsg (string-prefix? "no such table" errmsg)))

(define (busy-code? code)
  ;; SQLITE_BUSY = 5, SQLITE_LOCKED = 6.
  (memv code '(5 6)))

(define (run-stmt! db sql arg)
  "Prepare SQL, bind its single `?' to ARG, and step to completion.  Missing
tables (older browser schemas) are ignored; a busy/locked DB re-throws
'db-busy so the whole DB can be skipped."
  (catch 'sqlite-error
    (lambda ()
      (let ((stmt (sqlite-prepare db sql)))
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (sqlite-bind stmt 1 arg)
            (let loop () (when (sqlite-step stmt) (loop))))
          (lambda () (sqlite-finalize stmt)))))
    (lambda (key who code errmsg)
      (cond
       ((missing-table? errmsg) #f)
       ((busy-code? code) (throw 'db-busy sql))
       (else (throw key who code errmsg))))))

(define (count-rows db sql arg)
  (catch 'sqlite-error
    (lambda ()
      (let ((stmt (sqlite-prepare db sql)))
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (sqlite-bind stmt 1 arg)
            (let ((row (sqlite-step stmt)))
              (if row (vector-ref row 0) 0)))
          (lambda () (sqlite-finalize stmt)))))
    (lambda (key who code errmsg)
      (cond
       ((missing-table? errmsg) 0)
       ((busy-code? code) (throw 'db-busy sql))
       (else (throw key who code errmsg))))))

(define (scrub-db! db-path op patterns dry-run? touch-bookmarks? force-locked? log)
  "Apply OP against the single db at DB-PATH for every pattern.  A locked DB is
logged and skipped unless FORCE-LOCKED? is #t."
  (catch
    #t
    (lambda ()
      (let ((db (sqlite-open db-path SQLITE_OPEN_READWRITE)))
        (dynamic-wind
          (lambda () #f)
          (lambda ()
            (sqlite-busy-timeout db (if force-locked? 5000 300))
            ;; Enforce declared ON DELETE CASCADE relations.  SQLite defaults
            ;; foreign_keys=OFF per connection, so without this a real Firefox
            ;; places.sqlite is left with dangling references (e.g. orphaned
            ;; moz_places_metadata.referrer_place_id / moz_newtab_shortcuts_
            ;; interaction) after deleting a place — the manual `cascades' only
            ;; cover the non-FK child tables (visits, annos, inputhistory).
            (sqlite-exec db "PRAGMA foreign_keys = ON")
            (for-each
             (lambda (pat)
               (let* ((arg (string-append "%" pat "%"))
                      (n   (count-rows db (op-count-sql op touch-bookmarks?) arg)))
                 (when (> n 0)
                   (log "~a: ~a: ~a match(es) for ~s in ~a~%"
                        (ts) (assq-ref op 'table) n pat db-path))
                 (unless dry-run?
                   (for-each (lambda (sql) (run-stmt! db sql arg))
                             (op-cascade-delete-sqls op))
                   (run-stmt! db (op-primary-delete-sql op touch-bookmarks?) arg))))
             patterns))
          (lambda () (sqlite-close db)))))
    (lambda (key . args)
      (case key
        ((db-busy) (log "~a: SKIP locked db ~a~%" (ts) db-path))
        (else      (log "~a: ERROR on ~a: ~a ~a~%" (ts) db-path key args))))))

;;;
;;; Best-effort per-domain cache scan
;;;

(define (cache-scan! cache-globs patterns dry-run? grep home log)
  (for-each
   (lambda (glob)
     (for-each
      (lambda (dir)
        (for-each
         (lambda (pat)
           (catch #t
             (lambda ()
               (let* ((port (open-pipe* OPEN_READ grep "-rlZ" "-F" "--" pat dir))
                      (out  (read-delimited "" port)))
                 (close-pipe port)
                 (for-each
                  (lambda (f)
                    (unless (string-null? f)
                      (log "~a: cache hit ~a~%" (ts) f)
                      (unless dry-run?
                        (false-if-exception (delete-file f)))))
                  (string-split out #\nul))))
             (lambda (key . args)
               (log "~a: cache scan error in ~a: ~a~%" (ts) dir key))))
         patterns))
      (expand-glob home (glob-segments glob))))
   cache-globs))

;;;
;;; Profile discovery (also used by the inotify watcher)
;;;

(define* (profile-dirs #:key
                       (backends %browser-backends)
                       (extra-backends '())
                       (home (getenv "HOME")))
  (append-map
   (lambda (b)
     (append-map (lambda (g) (expand-glob home (glob-segments g)))
                 (assq-ref b 'profile-globs)))
   (append backends extra-backends)))

(define* (dirs-to-watch #:key
                        (backends %browser-backends)
                        (extra-backends '())
                        (home (getenv "HOME")))
  "Directories to hand to inotify.  We watch the whole profile dir because
Firefox writes WAL siblings (places.sqlite-wal) next to the DB."
  (profile-dirs #:backends backends #:extra-backends extra-backends #:home home))

(define* (db-files-to-watch #:key
                            (backends %browser-backends)
                            (extra-backends '())
                            (home (getenv "HOME")))
  (append-map
   (lambda (b)
     (let ((wdb (assq-ref b 'watch-db)))
       (filter-map
        (lambda (p)
          (let ((f (string-append p "/" wdb)))
            (and (file-exists? f) f)))
        (profile-dirs #:backends (list b) #:home home))))
   (append backends extra-backends)))

;;;
;;; Entry point
;;;

(define* (scrub! #:key
                 (patterns '())
                 (aspects '(history autocomplete cookies downloads))
                 (backends %browser-backends)
                 (extra-backends '())
                 (dry-run? #f)
                 (touch-bookmarks? #f)
                 (force-locked? #f)
                 (grep "grep")
                 (home (getenv "HOME"))
                 (log-port (current-output-port)))
  "Delete every browser trace matching PATTERNS across ASPECTS for all matching
profiles of BACKENDS (+ EXTRA-BACKENDS).  With DRY-RUN? nothing is deleted, only
match counts are logged."
  (define (log fmt . args) (apply format log-port fmt args))
  (cond
   ((null? patterns)
    (log "~a: browser-history-manager: no patterns configured, nothing to do~%"
         (ts)))
   (else
    (log "~a: browser-history-manager: patterns=~s aspects=~s dry-run=~a~%"
         (ts) patterns aspects dry-run?)
    (for-each
     (lambda (backend)
       (let ((sql-aspects (assq-ref backend 'sql-aspects))
             (cache-globs (assq-ref backend 'cache-globs))
             (profiles    (profile-dirs #:backends (list backend) #:home home)))
         (for-each
          (lambda (profile)
            (for-each
             (lambda (aspect)
               (cond
                ((eq? aspect 'cache)
                 (when cache-globs
                   (cache-scan! cache-globs patterns dry-run? grep home log)))
                (else
                 (let ((ops (assq-ref sql-aspects aspect)))
                   (when ops
                     (for-each
                      (lambda (op)
                        (let ((db-path (string-append profile "/"
                                                      (assq-ref op 'db))))
                          (when (file-exists? db-path)
                            (scrub-db! db-path op patterns
                                       dry-run? touch-bookmarks? force-locked?
                                       log))))
                      ops))))))
             aspects))
          profiles)))
     (append backends extra-backends)))))
