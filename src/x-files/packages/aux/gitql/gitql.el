;;; gitql.el --- Interactive GQL (Git Query Language) buffer -*- lexical-binding: t; -*-

;; Author: Grigory Shepelev <shegeley@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.5.0"))
;; Keywords: git tools
;; URL: https://github.com/AmrDeveloper/GQL

;;; Commentary:
;;
;; Comint-based interactive buffer for GQL with transient menus.
;; GQL is a SQL-like language to query Git repositories.
;;
;; Entry points:
;;   M-x gitql         — open session for current project / prompt
;;   M-x gitql-start   — open session for a specific repo
;;   M-x gitql-menu    — transient menu (also bound to C-c C-t in buffer)
;;
;; Inside a GitQL buffer:
;;   C-c C-t   transient menu
;;   C-c C-r   switch repository
;;   C-c C-c   interrupt

;;; Code:

(require 'comint)
(require 'transient)
(require 'project)

;;; Customization ─────────────────────────────────────────────────────────────

(defgroup gitql nil
  "Interactive GQL (Git Query Language) interface."
  :group 'tools
  :prefix "gitql-")

(defcustom gitql-executable "gql"
  "Path to the GQL executable."
  :type 'string
  :group 'gitql)

(defcustom gitql-prompt "gitql> "
  "Prompt string displayed in GitQL buffers."
  :type 'string
  :group 'gitql)

(defcustom gitql-known-repos nil
  "List of Git repository paths shown as completion candidates."
  :type '(repeat directory)
  :group 'gitql)

(defcustom gitql-default-limit 20
  "Default LIMIT appended to SELECT queries that omit one."
  :type 'integer
  :group 'gitql)

;;; Schema ─────────────────────────────────────────────────────────────────────

(defconst gitql-schema-alist
  '(("commits"
     commit_id title message author_name author_email
     committer_name committer_email datetime parents_count repo)
    ("branches"
     name commit_count is_head is_remote updated repo)
    ("tags"
     name repo)
    ("diffs"
     commit_id author_name author_email insertions removals
     files_changed diff_changes datetime repo)
    ("diffs_changes"
     commit_id insertions removals mode path repo)
    ("refs"
     name full_name type repo))
  "Alist of known GQL tables to their field symbols.
Reflects the schema of GQL 0.43.0.")

;;; Buffer-local state ────────────────────────────────────────────────────────

(defvar-local gitql--repo nil
  "Repository path used by the current GitQL buffer.")

;;; Utilities ─────────────────────────────────────────────────────────────────

(defun gitql--project-root ()
  "Return the current project root or nil."
  (when-let ((proj (project-current)))
    (project-root proj)))

(defun gitql--read-repo (&optional prompt)
  "Read a Git repository path, offering known repos and project root."
  (let* ((default (or gitql--repo (gitql--project-root) default-directory))
         (candidates (delete-dups
                      (delq nil (append (list default) gitql-known-repos)))))
    (expand-file-name
     (completing-read (or prompt "Repository: ")
                      candidates nil nil nil nil default))))

(defun gitql--buffer-name (repo)
  "Return a GitQL buffer name for REPO."
  (format "*GitQL[%s]*"
          (file-name-nondirectory (directory-file-name repo))))

;;; Font-lock ──────────────────────────────────────────────────────────────────

(defconst gitql-font-lock-keywords
  (let ((kw  '("SELECT" "FROM" "WHERE" "GROUP" "BY" "ORDER" "LIMIT" "OFFSET"
               "HAVING" "DISTINCT" "AS" "JOIN" "ON" "AND" "OR" "NOT" "IN"
               "LIKE" "BETWEEN" "IS" "NULL" "ASC" "DESC" "INNER" "LEFT"
               "RIGHT" "CROSS" "UNION" "ALL"))
        (fn  '("COUNT" "SUM" "AVG" "MIN" "MAX" "LENGTH" "LOWER" "UPPER"
               "TRIM" "NOW" "TYPEOF" "ROUND" "ABS"))
        (tbl '("commits" "branches" "tags" "diffs" "diffs_changes" "refs")))
    `((,(regexp-opt kw  'words) . font-lock-keyword-face)
      (,(regexp-opt fn  'words) . font-lock-function-name-face)
      (,(regexp-opt tbl 'words) . font-lock-type-face)
      ("'[^']*'" . font-lock-string-face)
      ("--.*$"   . font-lock-comment-face)))
  "Font-lock keywords for GitQL mode.")

;;; Completion ─────────────────────────────────────────────────────────────────

(defun gitql--completion-at-point ()
  "Complete GQL keywords and table names."
  (let ((kws (append
              '("SELECT" "FROM" "WHERE" "GROUP BY" "ORDER BY" "LIMIT"
                "OFFSET" "HAVING" "DISTINCT" "AS" "AND" "OR" "NOT"
                "IN" "LIKE" "BETWEEN" "IS NULL" "IS NOT NULL" "ASC" "DESC"
                "COUNT" "SUM" "AVG" "MIN" "MAX")
              '("commits" "branches" "tags" "diffs" "diffs_changes" "refs"))))
    (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
      (list (car bounds) (cdr bounds) kws
            :exclusive 'no))))

;;; Mode ───────────────────────────────────────────────────────────────────────

(defvar gitql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") #'gitql-menu)
    (define-key map (kbd "C-c C-r") #'gitql-set-repo)
    (define-key map (kbd "C-c C-c") #'comint-interrupt-subjob)
    map))

(define-derived-mode gitql-mode comint-mode "GitQL"
  "Major mode for an interactive GQL session.

Type SQL-like queries and press RET to execute them.

\\{gitql-mode-map}"
  (setq comint-prompt-regexp (concat "^" (regexp-quote gitql-prompt)))
  (setq comint-use-prompt-regexp t)
  (setq comint-process-echoes nil)
  (setq font-lock-defaults '(gitql-font-lock-keywords t t))
  (font-lock-mode 1)
  (add-hook 'completion-at-point-functions #'gitql--completion-at-point nil t))

;;; Process management ─────────────────────────────────────────────────────────

(defun gitql--repl-script ()
  "Return a bash script that provides a GQL REPL loop."
  ;; Print the initial prompt, then for every line read:
  ;;   • run gql if the line is non-empty
  ;;   • print the next prompt
  (concat "printf '" gitql-prompt "';"
          "while IFS= read -r _gitql_line; do"
          "  if [ -n \"$_gitql_line\" ]; then"
          "    \"$_GITQL_EXE\" -r \"$_GITQL_REPO\""
          "      -q \"$_gitql_line\" 2>&1;"
          "    printf '\\n';"
          "  fi;"
          "  printf '" gitql-prompt "';"
          "done"))

(defun gitql--spawn (buf repo)
  "Start the GQL REPL process in BUF for REPO."
  (let ((process-environment
         (append (list (format "_GITQL_REPO=%s" (expand-file-name repo))
                       (format "_GITQL_EXE=%s"  gitql-executable))
                 process-environment)))
    (make-comint-in-buffer "gitql" buf
                           "bash" nil "-c" (gitql--repl-script))))

;;; Public commands ─────────────────────────────────────────────────────────────

;;;###autoload
(defun gitql-start (repo)
  "Start or switch to a GitQL session for REPO."
  (interactive (list (gitql--read-repo)))
  (let* ((name (gitql--buffer-name repo))
         (buf  (get-buffer-create name)))
    (if (comint-check-proc buf)
        (pop-to-buffer buf)
      (with-current-buffer buf
        (gitql-mode)
        (setq gitql--repo repo)
        (gitql--spawn buf repo))
      (pop-to-buffer buf))))

;;;###autoload
(defun gitql ()
  "Open GitQL for the current project or prompt for a repository."
  (interactive)
  (gitql-start (or (gitql--project-root) (gitql--read-repo))))

(defun gitql-set-repo (repo)
  "Switch the current GitQL buffer to REPO, restarting the process."
  (interactive (list (gitql--read-repo "Switch to repository: ")))
  (unless (derived-mode-p 'gitql-mode)
    (user-error "Not in a GitQL buffer"))
  (when-let ((proc (get-buffer-process (current-buffer))))
    (kill-process proc))
  (setq gitql--repo repo)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize (format "\n-- repo: %s\n" repo)
                        'face 'font-lock-comment-face)))
  (gitql--spawn (current-buffer) repo))

(defun gitql-use-project-root ()
  "Set the GitQL repository to the current project root."
  (interactive)
  (if-let ((root (gitql--project-root)))
      (gitql-set-repo root)
    (user-error "No project found in current context")))

(defun gitql--send (query)
  "Insert QUERY into the GitQL buffer and submit it."
  (unless (derived-mode-p 'gitql-mode)
    (call-interactively #'gitql))
  (with-current-buffer (current-buffer)
    (goto-char (point-max))
    (insert (string-trim query))
    (comint-send-input)))

;;; Query templates ─────────────────────────────────────────────────────────────

(defmacro gitql--defquery (name docstring query)
  "Define command NAME that sends QUERY to the active GitQL buffer."
  `(defun ,name ()
     ,docstring
     (interactive)
     (unless (derived-mode-p 'gitql-mode) (call-interactively #'gitql))
     (gitql--send ,query)))

(gitql--defquery gitql-commits
  "Show the most recent commits."
  "SELECT commit_id, title, author_name, datetime FROM commits ORDER BY datetime DESC LIMIT 20")

(gitql--defquery gitql-branches
  "List all branches."
  "SELECT name, commit_count, is_head FROM branches")

(gitql--defquery gitql-tags
  "List all tags."
  "SELECT name FROM tags")

(gitql--defquery gitql-diffs
  "Show recent commit-level diff stats."
  "SELECT commit_id, insertions, removals, files_changed FROM diffs ORDER BY datetime DESC LIMIT 20")

(gitql--defquery gitql-diff-changes
  "Show recent file-level changes (path, insertions, removals)."
  "SELECT commit_id, path, insertions, removals, mode FROM diffs_changes LIMIT 30")

(gitql--defquery gitql-contributors
  "Show commit count per author."
  "SELECT author_name, COUNT(commit_id) AS commits FROM commits GROUP BY author_name ORDER BY commits DESC")

(gitql--defquery gitql-show-tables
  "List all available GQL tables."
  "SHOW TABLES")

(defun gitql-describe-table (table)
  "Show the schema for TABLE."
  (interactive
   (list (completing-read "Describe table: " (mapcar #'car gitql-schema-alist) nil t)))
  (unless (derived-mode-p 'gitql-mode) (call-interactively #'gitql))
  (gitql--send (concat "DESCRIBE " table)))

(defun gitql-custom-query (query)
  "Prompt for and send a custom GQL QUERY."
  (interactive
   (list (read-string "GQL: " "SELECT * FROM commits LIMIT 10")))
  (unless (derived-mode-p 'gitql-mode) (call-interactively #'gitql))
  (gitql--send query))

;;; Transient menu ──────────────────────────────────────────────────────────────

(transient-define-prefix gitql-menu ()
  "GitQL — main menu."
  [["Repository"
    ("r" "Set repository"   gitql-set-repo)
    ("s" "New session"      gitql-start)
    ("p" "Use project root" gitql-use-project-root)]
   ["Queries"
    ("c" "Commits"          gitql-commits)
    ("b" "Branches"         gitql-branches)
    ("t" "Tags"             gitql-tags)
    ("d" "Diffs (stats)"    gitql-diffs)
    ("f" "File changes"     gitql-diff-changes)
    ("a" "Authors / stats"  gitql-contributors)
    ("S" "Show tables"       gitql-show-tables)
    ("?" "Describe table"   gitql-describe-table)]
   ["Actions"
    ("q" "Custom query"     gitql-custom-query)
    ("k" "Clear buffer"     comint-clear-buffer)]])

;;; Entry point alias

;;;###autoload (autoload 'gitql-menu "gitql" nil t)

(provide 'gitql)
;;; gitql.el ends here
