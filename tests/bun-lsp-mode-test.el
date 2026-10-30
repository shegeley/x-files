;;; bun-lsp-mode-test.el --- bun-mode with lsp-mode: clients and end to end -*- lexical-binding: t; -*-
;; Needs lsp-mode, the real executables baked into bun-mode.el and the
;; TypeScript tree-sitter grammar.  Run through `make check/bun'.
(require 'ert)
(require 'cl-lib)
(require 'treesit)
(require 'lsp-mode)
(require 'lsp-javascript)
(require 'bun-mode)

(defmacro bun--test-in-file (file mode &rest body)
  "Run BODY in a buffer visiting FILE (not created) in major MODE."
  (declare (indent 2))
  `(with-temp-buffer
     (setq buffer-file-name ,file)
     (let ((treesit-auto-install-grammar 'never)) (,mode))
     (unwind-protect (progn ,@body) (setq buffer-file-name nil))))

(ert-deftest bun/test-lsp-mode-clients ()
  (let ((tsc (gethash 'bun-tsc lsp-clients))
        (biome (gethash 'bun-biome lsp-clients)))
    (should tsc)
    (should biome)
    (should (equal bun/modes (lsp--client-major-modes tsc)))
    (should (equal bun/modes (lsp--client-major-modes biome)))
    (should-not (lsp--client-add-on? tsc))
    (should (lsp--client-add-on? biome))
    ;; Wins over every stock client that also matches bun buffers.
    (dolist (id '(ts-ls tsgo deno-ls jsts-ls))
      (should (> (lsp--client-priority tsc) (lsp--client-priority (gethash id lsp-clients)))))
    (let ((bun/tsc-bin "/t/tsc") (bun/biome-bin "/b/biome"))
      (should (equal '("/t/tsc" "--lsp" "--stdio") (bun/lsp-tsc-command)))
      (should (equal '("/b/biome" "lsp-proxy") (bun/lsp-biome-command))))))

(ert-deftest bun/test-lsp-mode-register-idempotent ()
  (let ((lsp-language-id-configuration (copy-sequence lsp-language-id-configuration)))
    (bun/lsp-register)
    (bun/lsp-register)
    (dolist (mode bun/modes)
      (should (= 1 (cl-count mode lsp-language-id-configuration :key #'car))))))

(ert-deftest bun/test-lsp-mode-buffers ()
  (pcase-dolist (`(,mode ,id) '((bun/ts-mode "typescript")
                                (bun/tsx-mode "typescriptreact")
                                (bun/js-mode "javascript")
                                (bun/jsx-mode "javascriptreact")))
    (with-temp-buffer
      (setq buffer-file-name "/nonexistent/a.ts")
      (let ((treesit-auto-install-grammar 'never)) (funcall mode))
      (unwind-protect
          (progn
            (should (equal id (lsp-buffer-language)))
            (should (lsp--supports-buffer? (gethash 'bun-tsc lsp-clients)))
            (should (lsp--supports-buffer? (gethash 'bun-biome lsp-clients))))
        (setq buffer-file-name nil))))
  ;; Stock TypeScript buffers are left to the stock clients.
  (bun--test-in-file "/nonexistent/a.ts" typescript-ts-mode
    (should-not (lsp--supports-buffer? (gethash 'bun-tsc lsp-clients)))
    (should-not (lsp--supports-buffer? (gethash 'bun-biome lsp-clients)))))

(ert-deftest bun/test-lsp-mode-drop-formatting ()
  (let ((table (make-hash-table :test #'equal))
        (plist (list :hoverProvider t
                     :documentFormattingProvider t
                     :documentRangeFormattingProvider t)))
    (puthash "hoverProvider" t table)
    (puthash "documentFormattingProvider" t table)
    (puthash "documentOnTypeFormattingProvider" '(:firstTriggerCharacter ";") table)
    (bun--lsp-drop-formatting (make-lsp--workspace :server-capabilities table))
    (should (equal '("hoverProvider") (hash-table-keys table)))
    (bun--lsp-drop-formatting (make-lsp--workspace :server-capabilities plist))
    (should (plist-get plist :hoverProvider))
    (should-not (plist-get plist :documentFormattingProvider))
    (should-not (plist-get plist :documentRangeFormattingProvider))))

(ert-deftest bun/test-lsp-mode-no-null-capabilities ()
  (let ((sent (lsp--client-capabilities)))
    ;; The stock lsp-mode value really has the null (else drop this test).
    (should (assq 'inlineCompletion
                  (cdr (assq 'textDocument
                             (funcall (advice--cd*r (symbol-function 'lsp--client-capabilities)))))))
    (should-not (assq 'inlineCompletion (cdr (assq 'textDocument sent))))
    (should (assq 'hover (cdr (assq 'textDocument sent))))
    (should-not (string-match-p ":null\\|null[],}]" (lsp--json-serialize sent)))))

;;;; Live

(defun bun--test-lsp-wait (what predicate &optional timeout)
  "Spin until PREDICATE is non-nil or TIMEOUT (default 60s) passes."
  (let ((deadline (+ (float-time) (or timeout 60))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (or (funcall predicate) (ert-fail (format "Timed out waiting for %s" what)))))

(defun bun--test-lsp-diagnostics ()
  "Messages of every diagnostic lsp-mode holds for the current file."
  (mapcar (lambda (d) (format "[%s] %s" (lsp:diagnostic-source? d) (lsp:diagnostic-message d)))
          (gethash (lsp--fix-path-casing (buffer-file-name)) (lsp-diagnostics t))))

(ert-deftest bun/test-lsp-mode-end-to-end ()
  (should (treesit-language-available-p 'typescript))
  (let* ((treesit-auto-install-grammar 'never)
         (root (file-name-as-directory (make-temp-file "bun-lsp-mode-test" t)))
         (file (expand-file-name "a.ts" root))
         (lsp-session-file (expand-file-name ".lsp-session" root))
         (lsp-enable-file-watchers nil)
         (lsp-keep-workspace-alive nil)
         (lsp-auto-install-server nil)
         (lsp-restart 'ignore)
         (lsp-diagnostics-provider :none)
         (lsp-enabled-clients nil)
         buffer)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bun.lock" root) (insert "{}"))
          (with-temp-file (expand-file-name "package.json" root) (insert "{}"))
          (with-temp-file file
            (insert "const n: number = \"x\";\ndebugger;\nexport const   y  =  n\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should (eq major-mode 'bun/ts-mode))
            (lsp-workspace-folders-add root)
            (lsp)
            (bun--test-lsp-wait "both servers"
                                (lambda ()
                                  (equal '(bun-biome bun-tsc)
                                         (sort (mapcar (lambda (w) (lsp--client-server-id (lsp--workspace-client w)))
                                                       (cl-remove-if-not (lambda (w) (eq 'initialized (lsp--workspace-status w)))
                                                                         (lsp-workspaces)))
                                               #'string<))))
            ;; Types from tsc (pulled), lint from biome (pushed).
            (bun--test-lsp-wait "tsc + biome diagnostics"
                                (lambda ()
                                  (dolist (w (lsp-workspaces))
                                    (with-lsp-workspace w (lsp-diagnostics--request-pull-diagnostics w)))
                                  (let ((all (string-join (bun--test-lsp-diagnostics) "\n")))
                                    (and (string-match-p "not assignable to type 'number'" all)
                                         (string-match-p "debugger" all)))))
            ;; Hover comes from tsc.
            (let ((hover (lsp-request "textDocument/hover"
                                      (list :textDocument (lsp--text-document-identifier)
                                            :position (list :line 0 :character 6)))))
              (should (string-match-p "const n: number" (format "%S" hover))))
            ;; Only biome formats: exactly its output, no doubled edits from tsc.
            (should-not (lsp--capability :documentFormattingProvider
                                         (lsp--workspace-server-capabilities
                                          (cl-find 'bun-tsc (lsp-workspaces)
                                                   :key (lambda (w) (lsp--client-server-id (lsp--workspace-client w)))))))
            (lsp-format-buffer)
            (should (equal (buffer-string)
                           "const n: number = \"x\";\ndebugger;\nexport const y = n;\n"))
            ;; Biome's quick fixes are offered.
            (let ((actions (lsp-request "textDocument/codeAction"
                                        (list :textDocument (lsp--text-document-identifier)
                                              :range (list :start (list :line 1 :character 0)
                                                           :end (list :line 1 :character 9))
                                              :context (list :diagnostics [])))))
              (should (string-match-p "biome" (format "%S" actions))))
            (lsp-workspace-shutdown (car (lsp-workspaces)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (dolist (w (lsp--session-workspaces (lsp-session)))
        (ignore-errors (lsp-workspace-shutdown w)))
      (delete-directory root t))))

;;; bun-lsp-mode-test.el ends here
