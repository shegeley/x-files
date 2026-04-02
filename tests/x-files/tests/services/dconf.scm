(define-module (x-files tests services dconf)
  #:use-module ((x-files utils dconf) #:select (merge-dconf-entries))
  #:use-module (guix gexp)
  #:use-module (ares suitbl))

(define-test-suite merge-dconf-entries-tests
  (test-suite "static list merging"
    (test "disjoint sections"
      (is (equal?
            '(("org/gnome/GPaste"
               ("history-name" . "'history'"))
              ("org/gnome/shell"
               ("disable-user-extensions" . "false")))
            (merge-dconf-entries
             '(("org/gnome/GPaste"
                ("history-name" . "'history'")))
             '(("org/gnome/shell"
                ("disable-user-extensions" . "false")))))))

    (test "same section, no key conflict"
      (is (equal?
            '(("org/gnome/GPaste"
               ("history-name" . "'history'")
               ("track-extension-state" . "false")))
            (merge-dconf-entries
             '(("org/gnome/GPaste"
                ("history-name" . "'history'")))
             '(("org/gnome/GPaste"
                ("track-extension-state" . "false")))))))

    (test "same section, key override (second wins)"
      (is (equal?
            '(("org/gnome/GPaste"
               ("history-name" . "'new-history'")))
            (merge-dconf-entries
             '(("org/gnome/GPaste"
                ("history-name" . "'history'")))
             '(("org/gnome/GPaste"
                ("history-name" . "'new-history'")))))))

    (test "mixed: disjoint + overlapping sections"
      (is (equal?
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

    (test "empty inputs"
      (is (equal? '() (merge-dconf-entries '() '())))
      (is (equal?
            '(("org/gnome/GPaste" ("history-name" . "'history'")))
            (merge-dconf-entries
             '(("org/gnome/GPaste" ("history-name" . "'history'")))
             '())))
      (is (equal?
            '(("org/gnome/GPaste" ("history-name" . "'history'")))
            (merge-dconf-entries
             '()
             '(("org/gnome/GPaste" ("history-name" . "'history'"))))))))

  (test-suite "gexp inputs — falls back to build-time append"
    (test "gexp left → gexp result"
      (is (gexp?
            (merge-dconf-entries
             #~'(("org/gnome/GPaste" ("history-name" . "'history'")))
             '(("org/gnome/shell" ("disable-user-extensions" . "false")))))))

    (test "gexp right → gexp result"
      (is (gexp?
            (merge-dconf-entries
             '(("org/gnome/GPaste" ("history-name" . "'history'")))
             #~'(("org/gnome/shell" ("disable-user-extensions" . "false")))))))

    (test "both gexps → gexp result"
      (is (gexp?
            (merge-dconf-entries
             #~'(("org/gnome/GPaste" ("history-name" . "'history'")))
             #~'(("org/gnome/shell" ("disable-user-extensions" . "false")))))))

    (test "gexp + empty list → gexp result"
      (is (gexp?
            (merge-dconf-entries
             #~'(("org/gnome/GPaste" ("history-name" . "'history'")))
             '()))))))
