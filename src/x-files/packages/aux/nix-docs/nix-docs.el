;;; nix-docs.el --- Offline Nix documentation via Recoll -*- lexical-binding: t; -*-

;;; Commentary:
;; `nix-docs/at-point' shows excerpts; `nix-docs/search' searches all releases.
;; Eldoc uses the same asynchronous lookup, alongside Eglot or lsp-mode.
;; This is textual search, not Nix evaluation or semantic option resolution.

;;; Code:
(require 'cl-lib)
(require 'eldoc)
(require 'subr-x)
(require 'button)
(require 'url-util)
(require 'browse-url)
(require 'dom)
(require 'thingatpt)
(require 'treesit nil t)

(declare-function consult-recoll "consult-recoll")
(declare-function eww-open-file "eww")
(defvar consult-recoll-program)
(defvar consult-recoll-search-flags)
(defvar consult-recoll-open-fn)
(defvar consult-recoll-inline-snippets)

(defgroup nix-docs nil "Offline Nix documentation." :group 'tools)
(defcustom nix-docs/directory nil
  "Directory containing the packaged HTML manuals."
  :type '(choice (const nil) directory))
(defcustom nix-docs/index-directory nil
  "Recoll configuration directory containing the prebuilt index."
  :type '(choice (const nil) directory))
(defcustom nix-docs/program "recollq"
  "Recoll query executable, set to a store path by feature-nix-dev."
  :type 'file)
(defcustom nix-docs/timeout 10
  "Maximum seconds for an asynchronous documentation query."
  :type 'number)
(defcustom nix-docs/nixos-release "26.05"
  "NixOS release used for at-point excerpts; nil searches all releases.
`nix-docs/search' always searches all indexed releases."
  :type '(choice (const nil) string))

(defvar nix-docs/cache (make-hash-table :test #'equal))
(defvar-local nix-docs/request nil)
(defvar-local nix-docs/saved-strategy nil)
(defvar nix-docs/mode)

(defun nix-docs/static-path (node)
  "Read a static identifier path from tree-sitter NODE; never evaluate Nix."
  (pcase (and node (treesit-node-type node))
    ("identifier" (treesit-node-text node t))
    ("variable_expression"
     (nix-docs/static-path (treesit-node-child-by-field-name node "name")))
    ("attrpath"
     (let ((parts (mapcar #'nix-docs/static-path (treesit-node-children node t))))
       (when (and parts (cl-every #'identity parts)) (string-join parts "."))))
    ("select_expression"
     (let ((base (nix-docs/static-path (treesit-node-child-by-field-name node "expression")))
           (path (nix-docs/static-path (treesit-node-child-by-field-name node "attrpath"))))
       (when (and base path) (concat base "." path))))))

(defun nix-docs/tree-identifier ()
  "Read the identifier at point using the Nix parse tree.
Directly nested literal attribute sets contribute their enclosing names.
Dynamic attributes, comments and literal strings are deliberately ignored."
  (let* ((node (treesit-node-at (point) 'nix))
         (parent (and node (treesit-node-parent node))))
    (when (and node (equal (treesit-node-type node) "identifier")
               (<= (treesit-node-start node) (point))
               (<= (point) (treesit-node-end node)))
      (when (member (treesit-node-type parent) '("attrpath" "variable_expression"))
        (setq node parent parent (treesit-node-parent parent)))
      (when (equal (treesit-node-type parent) "select_expression")
        (setq node parent parent (treesit-node-parent parent)))
      (let ((path (nix-docs/static-path node)))
        (when (and path (equal (treesit-node-type parent) "binding")
                   (treesit-node-eq node (treesit-node-child-by-field-name parent "attrpath")))
          (let ((binding parent) outer)
            (while
                (let* ((set (treesit-node-parent binding))
                       (expression (and set (treesit-node-parent set))))
                  (setq outer (and expression (treesit-node-parent expression)))
                  (and (equal (treesit-node-type set) "binding_set")
                       (member (treesit-node-type expression) '("attrset_expression" "rec_attrset_expression"))
                       (equal (treesit-node-type outer) "binding")))
              (let ((prefix (nix-docs/static-path
                             (treesit-node-child-by-field-name outer "attrpath"))))
                (setq path (and path prefix (concat prefix "." path))
                      binding outer)))))
        path))))

(defun nix-docs/identifier-at-point ()
  "Get a Nix identifier using tree-sitter, or a symbol if no parser exists."
  (let ((term
         (if (and (fboundp 'treesit-parser-list)
                  (cl-find 'nix (treesit-parser-list) :key #'treesit-parser-language))
             (nix-docs/tree-identifier)
           (unless (nth 8 (syntax-ppss)) (thing-at-point 'symbol t)))))
    (unless (member term '("let" "in" "with" "if" "then" "else" "true" "false" "null"))
      term)))

(defun nix-docs/query (term)
  "Construct a literal phrase query for identifier TERM."
  (unless (string-match-p "\\`[[:alnum:]_'.-]+\\'" term)
    (user-error "Not a Nix identifier: %s" term))
  (let ((query (format "\"%s\"" (string-remove-prefix "pkgs." term))))
    (if nix-docs/nixos-release
        (format "%s (dir:nixos-%s OR filename:nix-*.html OR filename:nixpkgs-*.html)"
                query nix-docs/nixos-release)
      query)))

(defun nix-docs/decode (output)
  "Decode Recoll's base64 URL, title and abstract records in OUTPUT."
  (cl-loop for line in (split-string output "\n" t)
           for fields = (split-string (string-remove-suffix " " line) " " nil)
           when (and (= (length fields) 3)
                     (cl-every (lambda (field)
                                 (string-match-p "\\`[A-Za-z0-9+/=]*\\'" field))
                               fields))
           append
           (condition-case nil
               (pcase-let ((`(,url ,title ,excerpt)
                            (mapcar (lambda (field)
                                      (decode-coding-string
                                       (base64-decode-string field) 'utf-8))
                                    fields)))
                 (when (string-prefix-p "file://" url)
                   (list `((url . ,url) (title . ,title)
                           (excerpt . ,(if (libxml-available-p)
                                           (with-temp-buffer
                                             (insert "<body>" excerpt "</body>")
                                             (dom-texts (libxml-parse-html-region (point-min) (point-max))))
                                         excerpt))))))
             (error nil))))

(defun nix-docs/lookup (query callback)
  "Asynchronously look up QUERY; call CALLBACK with (HITS ERROR).
Successful results, including misses, are cached.  Errors are not cached."
  (let* ((key (list nix-docs/index-directory query))
         (cached (gethash key nix-docs/cache 'missing)))
    (cond
     ((not (eq cached 'missing)) (funcall callback cached nil) nil)
     ((not (and nix-docs/index-directory
                (file-readable-p (expand-file-name "recoll.conf" nix-docs/index-directory))))
      (funcall callback nil "Recoll index is not configured") nil)
     (t
      (let ((output (generate-new-buffer " *nix-docs-query*"))
            (errors (generate-new-buffer " *nix-docs-errors*")))
        (condition-case err
            (let* ((process
                    (make-process
                     :name "nix-docs" :buffer output :stderr errors
                     :noquery t :connection-type 'pipe :coding 'utf-8-unix
                     :command (list nix-docs/program "-c" nix-docs/index-directory
                                    "-n" "3" "-F" "url title abstract"
                                    query)
                     :sentinel
                     (lambda (process _event)
                       (when (memq (process-status process) '(exit signal))
                         (when-let ((timer (process-get process 'timer)))
                           (cancel-timer timer))
                         (let* ((ok (and (eq (process-status process) 'exit)
                                         (= 0 (process-exit-status process))))
                                (hits (and ok (with-current-buffer output
                                                (nix-docs/decode (buffer-string))))))
                           (when ok
                             (when (>= (hash-table-count nix-docs/cache) 256)
                               (clrhash nix-docs/cache))
                             (puthash key hits nix-docs/cache))
                           (unwind-protect
                               (funcall callback hits
                                        (unless ok
                                          (if (process-get process 'timed-out)
                                              "Recoll query timed out"
                                            (format "Recoll exited %s: %s"
                                                    (process-exit-status process)
                                                    (with-current-buffer errors
                                                      (string-trim (buffer-string)))))))
                             (kill-buffer output)
                             (kill-buffer errors))))))))
              (process-put process 'timer
                           (run-at-time nix-docs/timeout nil
                                        (lambda ()
                                          (when (process-live-p process)
                                            (process-put process 'timed-out t)
                                            (delete-process process)))))
              process)
          (error
           (kill-buffer output)
           (kill-buffer errors)
           (funcall callback nil (error-message-string err)) nil)))))))

(defun nix-docs/label (hit)
  "Include the manual version in the label for HIT."
  (let* ((url (alist-get 'url hit))
         (file (url-unhex-string (string-remove-prefix "file://" url))))
    (format "%s — %s" (alist-get 'title hit)
            (file-relative-name file nix-docs/directory))))

(defun nix-docs/format-hits (hits)
  "Format HITS as short plain-text documentation."
  (mapconcat (lambda (hit)
               (format "%s\n%s" (nix-docs/label hit) (alist-get 'excerpt hit)))
             hits "\n\n"))

;;;###autoload
(defun nix-docs/open (&optional browser)
  "Open the small offline manual index in EWW, or BROWSER with prefix."
  (interactive "P")
  (unless nix-docs/directory (user-error "Nix manuals are not configured"))
  (let ((file (expand-file-name "index.html" nix-docs/directory)))
    (if browser (browse-url-of-file file)
      (require 'eww)
      (eww-open-file file))))

;;;###autoload
(defun nix-docs/search (&optional snippets)
  "Quickly search all manual releases with Consult.
With prefix SNIPPETS, also extract excerpts (slower for large HTML files).
Selecting a result opens its full HTML in a browser."
  (interactive "P")
  (require 'consult-recoll)
  (unless nix-docs/index-directory (user-error "Nix index is not configured"))
  (let ((consult-recoll-program nix-docs/program)
        (consult-recoll-search-flags (append (list "-c" nix-docs/index-directory)
                                            (when snippets '("-A" "-p" "5"))))
        (consult-recoll-inline-snippets nil)
        (consult-recoll-open-fn #'browse-url-of-file))
    (consult-recoll nil)))

;;;###autoload
(defun nix-docs/at-point ()
  "Show offline manual excerpts for the identifier at point."
  (interactive)
  (let ((term (or (nix-docs/identifier-at-point) (user-error "No Nix identifier at point"))))
    (nix-docs/lookup
     (nix-docs/query term)
     (lambda (hits error)
       (if (or error (not hits))
           (message "%s" (or error (format "No manual matches for %s" term)))
         (with-help-window "*Nix documentation*"
           (with-current-buffer standard-output
             (insert (format "%s — фрагменты руководств (Recoll)\n\n" term))
             (dolist (hit hits)
               (let ((file (url-unhex-string
                            (string-remove-prefix "file://" (alist-get 'url hit)))))
                 (insert (nix-docs/label hit) "\n" (alist-get 'excerpt hit) "\n")
                 (insert-text-button "Полный HTML в браузере"
                                     'action (lambda (_) (browse-url-of-file file)))
                 (insert " · ")
                 (insert-text-button "EWW (большой файл может быть медленным)"
                                     'action (lambda (_) (require 'eww) (eww-open-file file)))
                 (insert "\n\n"))))))))))

(defun nix-docs/eldoc (callback &rest _ignored)
  "Supply asynchronous manual excerpts to Eldoc CALLBACK."
  (when-let ((term (nix-docs/identifier-at-point)))
    (let ((buffer (current-buffer))
          (position (point))
          (tick (buffer-chars-modified-tick)))
      (when (process-live-p nix-docs/request)
        (delete-process nix-docs/request))
      (setq nix-docs/request
            (nix-docs/lookup
             (nix-docs/query term)
             (lambda (hits _error)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and nix-docs/mode (= position (point))
                              (= tick (buffer-chars-modified-tick)))
                     (funcall callback (and hits (concat "Recoll: фрагменты руководств\n"
                                                        (nix-docs/format-hits hits)))
                              :thing term
                              :echo (and hits (truncate-string-to-width
                                               (alist-get 'excerpt (car hits)) 160 nil nil "…")))))))))
      t)))

;;;###autoload
(define-minor-mode nix-docs/mode
  "Add offline manual excerpts to Eldoc without replacing the LSP provider."
  :lighter " NixDoc"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-d") #'nix-docs/at-point)
            map)
  (if nix-docs/mode
      (progn
        (when (and (fboundp 'treesit-language-available-p)
                   (treesit-language-available-p 'nix))
          (treesit-parser-create 'nix))
        (unless nix-docs/saved-strategy
          (setq nix-docs/saved-strategy (list eldoc-documentation-strategy)))
        (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
        (add-hook 'eldoc-documentation-functions #'nix-docs/eldoc t t))
    (remove-hook 'eldoc-documentation-functions #'nix-docs/eldoc t)
    (when nix-docs/saved-strategy
      (setq-local eldoc-documentation-strategy (car nix-docs/saved-strategy))
      (setq nix-docs/saved-strategy nil))
    (when (process-live-p nix-docs/request) (delete-process nix-docs/request))))

(defun nix-docs/enable ()
  "Enable offline documentation in Nix buffers, including after LSP setup."
  (when (derived-mode-p 'nix-mode 'nix-ts-mode) (nix-docs/mode 1)))

(provide 'nix-docs)
;;; nix-docs.el ends here
