;;; gitql-test.el --- ERT tests for gitql.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests that guard template queries against schema regressions:
;; every column referenced in a SELECT must exist in the FROM table
;; according to `gitql-schema-alist'.

;;; Code:

(require 'ert)
(require 'gitql)

;;; Schema helpers ─────────────────────────────────────────────────────────────

(defun gitql-test--known-tables ()
  "Return list of known table name strings."
  (mapcar #'car gitql-schema-alist))

(defun gitql-test--table-fields (table)
  "Return field symbol list for TABLE, or nil if TABLE is unknown."
  (cdr (assoc table gitql-schema-alist)))

(defun gitql-test--parse-from (query)
  "Return the table name string from QUERY's FROM clause, or nil."
  (when (string-match
         (rx bow "FROM" eow (+ space) (group (+ (any alnum "_"))))
         (upcase-initials (upcase query)))
    (downcase (match-string 1 (upcase query))))
  ;; redo with case-insensitive
  (when (string-match
         "\\bFROM\\b[[:space:]]+\\([A-Za-z_]+\\)" query)
    (match-string 1 query)))

(defun gitql-test--parse-select-columns (query)
  "Return list of column name strings from QUERY's SELECT clause.
Returns nil for wildcard (*) queries."
  (when (string-match
         "\\bSELECT\\b[[:space:]]+\\(.*?\\)[[:space:]]+\\bFROM\\b" query)
    (let ((cols (string-trim (match-string 1 query))))
      (unless (string= cols "*")
        (mapcar (lambda (c)
                  ;; strip AS alias  e.g. "COUNT(commit_id) AS commits" → "commit_id"
                  (let ((raw (string-trim c)))
                    (if (string-match "([[:space:]]*\\([A-Za-z_]+\\)" raw)
                        (match-string 1 raw)
                      (car (split-string raw "[[:space:]]")))))
                (split-string cols ","))))))

(defun gitql-test--validate-query (query)
  "Validate QUERY against `gitql-schema-alist'.
Return a list of error strings, empty when the query is valid."
  (let* ((table   (gitql-test--parse-from query))
         (columns (gitql-test--parse-select-columns query))
         (known   (gitql-test--table-fields table))
         (errors  '()))
    (unless table
      (push (format "Cannot parse FROM clause in: %s" query) errors))
    (when (and table (not (member table (gitql-test--known-tables))))
      (push (format "Unknown table `%s'" table) errors))
    (when (and table known columns)
      (dolist (col columns)
        (unless (memq (intern col) known)
          (push (format "Column `%s' not in table `%s'" col table) errors))))
    (nreverse errors)))

;;; Template query tests ────────────────────────────────────────────────────────

(defmacro gitql-deftest-query (name query)
  "Define an ERT test asserting QUERY passes schema validation."
  `(ert-deftest ,name ()
     ,(format "Schema-validate the `%s' template query." name)
     (let ((errs (gitql-test--validate-query ,query)))
       (should-not errs)
       (when errs
         (ert-fail (mapconcat #'identity errs "\n"))))))

(gitql-deftest-query
 gitql-test-commits-query
 "SELECT commit_id, title, author_name, datetime FROM commits ORDER BY datetime DESC LIMIT 20")

(gitql-deftest-query
 gitql-test-branches-query
 "SELECT name, commit_count, is_head FROM branches")

(gitql-deftest-query
 gitql-test-tags-query
 "SELECT name FROM tags")

(gitql-deftest-query
 gitql-test-diffs-query
 "SELECT commit_id, insertions, removals, files_changed FROM diffs ORDER BY datetime DESC LIMIT 20")

(gitql-deftest-query
 gitql-test-diff-changes-query
 "SELECT commit_id, path, insertions, removals, mode FROM diffs_changes LIMIT 30")

(gitql-deftest-query
 gitql-test-contributors-query
 "SELECT author_name, COUNT(commit_id) AS commits FROM commits GROUP BY author_name ORDER BY commits DESC")

;;; Schema-alist sanity ─────────────────────────────────────────────────────────

(ert-deftest gitql-test-schema-alist-structure ()
  "Every entry in `gitql-schema-alist' is (string . symbol-list)."
  (dolist (entry gitql-schema-alist)
    (should (stringp (car entry)))
    (should (listp   (cdr entry)))
    (dolist (field (cdr entry))
      (should (symbolp field)))))

(ert-deftest gitql-test-known-tables-match-show-tables ()
  "Tables in `gitql-schema-alist' match those returned by SHOW TABLES (0.43.0)."
  (let ((expected (sort (list "commits" "branches" "tags"
                              "diffs" "diffs_changes" "refs")
                        #'string<))
        (actual   (sort (gitql-test--known-tables) #'string<)))
    (should (equal expected actual))))

(ert-deftest gitql-test-describe-table-candidates ()
  "`gitql-describe-table' offers only known table names."
  (should (equal (sort (mapcar #'car gitql-schema-alist) #'string<)
                 (sort (gitql-test--known-tables) #'string<))))

;;; Utility tests ──────────────────────────────────────────────────────────────

(ert-deftest gitql-test-buffer-name ()
  "Buffer name is derived from the last path component."
  (should (equal "*GitQL[x-files]*"
                 (gitql--buffer-name "/home/user/Projects/x-files")))
  (should (equal "*GitQL[x-files]*"
                 (gitql--buffer-name "/home/user/Projects/x-files/"))))

(ert-deftest gitql-test-repl-script-contains-exe-and-repo ()
  "REPL script references both env vars."
  (let ((script (gitql--repl-script)))
    (should (string-match-p "_GITQL_EXE"  script))
    (should (string-match-p "_GITQL_REPO" script))))

(provide 'gitql-test)
;;; gitql-test.el ends here
