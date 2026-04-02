(define-module (x-files tests services dconf)
  #:use-module ((x-files utils dconf) #:select (merge-dconf-entries))

  #:use-module (guix gexp)

  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext test))

;;;
;;; merge-dconf-entries: pure function tests (no heavy package imports)
;;;

(define-test merge-dconf-entries-disjoint-sections-
  (test-group "merge-dconf-entries: disjoint sections"
    (test-equal
      '(("org/gnome/GPaste"
         ("history-name" . "'history'"))
        ("org/gnome/shell"
         ("disable-user-extensions" . "false")))
      (merge-dconf-entries
       '(("org/gnome/GPaste"
          ("history-name" . "'history'")))
       '(("org/gnome/shell"
          ("disable-user-extensions" . "false")))))))

(define-test merge-dconf-entries-same-section-
  (test-group "merge-dconf-entries: same section, no key conflict"
    (test-equal
      '(("org/gnome/GPaste"
         ("history-name" . "'history'")
         ("track-extension-state" . "false")))
      (merge-dconf-entries
       '(("org/gnome/GPaste"
          ("history-name" . "'history'")))
       '(("org/gnome/GPaste"
          ("track-extension-state" . "false")))))))

(define-test merge-dconf-entries-key-override-
  (test-group "merge-dconf-entries: same section, key override (second wins)"
    (test-equal
      '(("org/gnome/GPaste"
         ("history-name" . "'new-history'")))
      (merge-dconf-entries
       '(("org/gnome/GPaste"
          ("history-name" . "'history'")))
       '(("org/gnome/GPaste"
          ("history-name" . "'new-history'")))))))

(define-test merge-dconf-entries-mixed-
  (test-group "merge-dconf-entries: mixed — disjoint + overlapping sections"
    (test-equal
      '(("org/gnome/GPaste"
         ("history-name" . "'history'")
         ("track-extension-state" . "false"))
        ("org/gnome/shell"
         ("disable-user-extensions" . "false"))
        ("org/gnome/desktop"
         ("color-scheme" . "'prefer-dark'")))
      (merge-dconf-entries
       '(("org/gnome/GPaste"
          ("history-name" . "'history'"))
         ("org/gnome/shell"
          ("disable-user-extensions" . "false")))
       '(("org/gnome/GPaste"
          ("track-extension-state" . "false"))
         ("org/gnome/desktop"
          ("color-scheme" . "'prefer-dark'")))))))

(define-test merge-dconf-entries-empty-
  (test-group "merge-dconf-entries: empty inputs"
    (test-equal '() (merge-dconf-entries '() '()))
    (test-equal
      '(("org/gnome/GPaste" ("history-name" . "'history'")))
      (merge-dconf-entries
       '(("org/gnome/GPaste" ("history-name" . "'history'")))
       '()))
    (test-equal
      '(("org/gnome/GPaste" ("history-name" . "'history'")))
      (merge-dconf-entries
       '()
       '(("org/gnome/GPaste" ("history-name" . "'history'")))))))

;;;
;;; merge-dconf-entries: gexp inputs
;;;
;;; When either input is a gexp (entries containing store paths or other
;;; build-time values), static deep-merging is impossible.  The result is
;;; a gexp that appends both entry lists at build time.
;;;

(define-test merge-dconf-entries-gexp-left-
  (test-group "merge-dconf-entries: gexp left → gexp result"
    (test-assert
      (gexp?
       (merge-dconf-entries
        #~'(("org/gnome/GPaste" ("history-name" . "'history'")))
        '(("org/gnome/shell" ("disable-user-extensions" . "false"))))))))

(define-test merge-dconf-entries-gexp-right-
  (test-group "merge-dconf-entries: gexp right → gexp result"
    (test-assert
      (gexp?
       (merge-dconf-entries
        '(("org/gnome/GPaste" ("history-name" . "'history'")))
        #~'(("org/gnome/shell" ("disable-user-extensions" . "false"))))))))

(define-test merge-dconf-entries-gexp-both-
  (test-group "merge-dconf-entries: both gexps → gexp result"
    (test-assert
      (gexp?
       (merge-dconf-entries
        #~'(("org/gnome/GPaste" ("history-name" . "'history'")))
        #~'(("org/gnome/shell" ("disable-user-extensions" . "false"))))))))

(define-test merge-dconf-entries-gexp-empty-list-
  (test-group "merge-dconf-entries: gexp + empty list → gexp result"
    (test-assert
      (gexp?
       (merge-dconf-entries
        #~'(("org/gnome/GPaste" ("history-name" . "'history'")))
        '())))))
