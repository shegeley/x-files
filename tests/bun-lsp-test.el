;;; bun-lsp-test.el --- bun-mode eglot end to end (tsc + biome via rass) -*- lexical-binding: t; -*-
;; Needs the real executables baked into bun-mode.el and the TypeScript
;; tree-sitter grammar.  Run through `make check/bun'.
(require 'ert)
(require 'cl-lib)
(require 'eglot)
(require 'flymake)
(require 'treesit)
(require 'bun-mode)

(defvar bun-test/source
  "const n: number = \"x\";\ndebugger;\nexport const   y  =  n\n")

(defun bun-test/wait (what predicate &optional timeout)
  "Spin until PREDICATE is non-nil or TIMEOUT (default 60s) passes."
  (let ((deadline (+ (float-time) (or timeout 60))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.2))
    (or (funcall predicate) (ert-fail (format "Timed out waiting for %s" what)))))

(defun bun-test/diagnostics ()
  ;; Batch Emacs has no idle timers to run flymake on its own.
  (flymake-start)
  (mapcar (lambda (d) (format "%s" (flymake-diagnostic-text d)))
          (flymake-diagnostics)))

(ert-deftest bun-lsp/test-end-to-end ()
  (should (treesit-language-available-p 'typescript))
  (let* ((treesit-auto-install-grammar 'never)
         (root (file-name-as-directory (make-temp-file "bun-lsp-test" t)))
         (file (expand-file-name "a.ts" root))
         (eglot-sync-connect t)
         (eglot-autoshutdown nil)
         buffer)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bun.lock" root) (insert "{}"))
          (with-temp-file (expand-file-name "package.json" root) (insert "{}"))
          (with-temp-file file (insert bun-test/source))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (should (eq major-mode 'bun-ts-mode))
            ;; `eglot-ensure' waits for a command loop batch never runs.
            (apply #'eglot (eglot--guess-contact))
            (bun-test/wait "eglot" (lambda () (eglot-current-server)))
            (let ((server (eglot-current-server)))
              (should (equal "typescript" (plist-get (eglot--TextDocumentItem) :languageId)))
              ;; Types from tsc, lint from biome, merged by rass.
              (bun-test/wait "tsc + biome diagnostics"
                             (lambda ()
                               (let ((all (string-join (bun-test/diagnostics) "\n")))
                                 (and (string-match-p "not assignable to type 'number'" all)
                                      (string-match-p "debugger" all)))))
              ;; Hover is answered by the primary (tsc).
              (let ((hover (jsonrpc-request
                            server :textDocument/hover
                            (list :textDocument (eglot--TextDocumentIdentifier)
                                  :position (list :line 0 :character 6)))))
                (should (string-match-p "const n: number"
                                        (format "%S" (plist-get hover :contents)))))
              ;; Formatting goes to biome: it adds the semicolon tsc would not.
              (eglot-format-buffer)
              (should (string-match-p "^export const y = n;$" (buffer-string)))
              ;; Biome's quick fix for `debugger' is offered alongside tsc's.
              (goto-char (point-min))
              (forward-line 1)
              (let ((actions (jsonrpc-request
                              server :textDocument/codeAction
                              (list :textDocument (eglot--TextDocumentIdentifier)
                                    :range (list :start (list :line 1 :character 0)
                                                 :end (list :line 1 :character 9))
                                    :context (list :diagnostics [])))))
                (should (cl-some (lambda (a) (string-match-p "biome" (format "%S" a)))
                                 actions)))
              (eglot-shutdown server))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

;;; bun-lsp-test.el ends here
