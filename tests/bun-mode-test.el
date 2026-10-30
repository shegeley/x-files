;;; bun-mode-test.el --- Offline bun-mode tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'eglot)
(require 'treesit)
(require 'bun-mode)

(defmacro bun--test-with-tree (spec &rest body)
  "Run BODY with `root' bound to a temp dir holding SPEC files (name . text)."
  (declare (indent 1))
  `(let ((root (file-name-as-directory (make-temp-file "bun-mode-test" t))))
     (unwind-protect
         (progn
           (dolist (file ,spec)
             (let ((path (expand-file-name (car file) root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path (insert (cdr file)))))
           ,@body)
       (delete-directory root t))))

(defun bun--test-mode-of (file)
  "Major mode `set-auto-mode' picks for FILE."
  (with-temp-buffer
    (setq buffer-file-name file)
    ;; Emacs 31+ would prompt to install a missing grammar.
    (let ((enable-local-variables nil)
          (treesit-auto-install-grammar 'never))
      (set-auto-mode))
    (prog1 major-mode (setq buffer-file-name nil))))

(define-derived-mode bun--test-fallback-mode prog-mode "Fallback")

(defun bun--test-first-handler (file)
  "Mode function of the first `auto-mode-alist' entry matching FILE."
  (cdr (seq-find (lambda (entry) (and (stringp (car entry)) (string-match-p (car entry) file)))
                 auto-mode-alist)))

(ert-deftest bun/test-project-root ()
  (dolist (marker bun/project-markers)
    (bun--test-with-tree `((,marker . "") ("src/deep/a.ts" . ""))
      (should (equal root (bun/project-root (expand-file-name "src/deep/" root))))))
  (bun--test-with-tree '(("package.json" . "{}") ("a.ts" . ""))
    (should-not (bun/project-root root))))

(ert-deftest bun/test-modes-inside-bun-project ()
  (bun--test-with-tree '(("bun.lock" . "") ("a.ts" . ""))
    (dolist (case '(("a.ts" . bun/ts-mode) ("b.mts" . bun/ts-mode) ("c.cts" . bun/ts-mode)
                    ("d.tsx" . bun/tsx-mode) ("e.js" . bun/js-mode) ("f.mjs" . bun/js-mode)
                    ("g.cjs" . bun/js-mode) ("h.jsx" . bun/jsx-mode)))
      (should (eq (cdr case) (bun--test-mode-of (expand-file-name (car case) root)))))
    (should (provided-mode-derived-p 'bun/ts-mode 'typescript-ts-mode))
    (should (provided-mode-derived-p 'bun/tsx-mode 'tsx-ts-mode))
    (should (provided-mode-derived-p 'bun/js-mode 'js-ts-mode))))

(ert-deftest bun/test-fallback-outside-bun-project ()
  (let ((auto-mode-alist (cons '("\\.ts\\'" . bun--test-fallback-mode) auto-mode-alist)))
    (bun/install-auto-mode)
    (bun--test-with-tree '(("package.json" . "{}"))
      (should (eq 'bun--test-fallback-mode
                  (bun--test-mode-of (expand-file-name "a.ts" root)))))))

(ert-deftest bun/test-stays-ahead-of-deno-mode ()
  (let ((auto-mode-alist (copy-sequence auto-mode-alist))
        (features (copy-sequence features))
        (after-load-alist (copy-tree after-load-alist)))
    ;; What deno-mode.el does on load: prepend its dispatcher.
    (push '("\\.ts\\'" . bun--test-fallback-mode) auto-mode-alist)
    (should (eq 'bun--test-fallback-mode (bun--test-first-handler "a.ts")))
    (should-not (featurep 'deno-mode))
    ;; bun-mode's `with-eval-after-load' alone must restore the order.
    (provide 'deno-mode)
    (should (eq 'bun/mode-maybe (bun--test-first-handler "a.ts")))
    (bun/install-auto-mode)
    (should (= 1 (cl-count '("\\.[cm]?ts\\'" . bun/mode-maybe) auto-mode-alist :test #'equal)))
    (bun--test-with-tree '(("bun.lock" . ""))
      (should (eq 'bun/ts-mode (bun--test-mode-of (expand-file-name "a.ts" root)))))
    (bun--test-with-tree '(("deno.json" . "{}"))
      (should (eq 'bun--test-fallback-mode (bun--test-mode-of (expand-file-name "a.ts" root)))))))

(ert-deftest bun/test-eglot-server ()
  (let ((eglot-server-programs
         (cons '((typescript-ts-mode tsx-ts-mode js-ts-mode) . ("generic-ls"))
               eglot-server-programs)))
    ;; Re-run the registration on top of a later generic entry, twice.
    (bun/eglot-register)
    (bun/eglot-register)
    (should (= 1 (cl-count #'bun/eglot-contact eglot-server-programs :key #'cdr)))
    (pcase-dolist (`(,mode ,id) '((bun/ts-mode "typescript")
                                  (bun/tsx-mode "typescriptreact")
                                  (bun/js-mode "javascript")
                                  (bun/jsx-mode "javascriptreact")))
      (pcase-let ((`(,_modes . ,contact) (eglot--lookup-mode mode)))
        (should (eq contact #'bun/eglot-contact)))
      (should (equal id (plist-get (cdr (assq mode bun/eglot-modes)) :language-id))))
    (should (eq 'typescript-ts-mode (caar (car (eglot--lookup-mode 'typescript-ts-mode)))))))

(ert-deftest bun/test-eglot-contact ()
  (let ((bun/rass-bin "/r/rass") (bun/tsc-bin "/t/tsc") (bun/biome-bin "/b/biome"))
    (should (equal (bun/eglot-contact)
                   (list "/r/rass" "--log-level" "warn" bun/rass-preset
                         "--" "/t/tsc" "--lsp" "--stdio"
                         "--" "/b/biome" "lsp-proxy")))
    (should (file-exists-p bun/rass-preset))))

(ert-deftest bun/test-commands ()
  (bun--test-with-tree '(("bun.lock" . "")
                        ("package.json" . "{\"scripts\": {\"dev\": \"x\", \"build\": \"y\"}}")
                        ("src/a.ts" . ""))
    (let ((bun/bin "/s/bun") calls)
      (cl-letf (((symbol-function 'compile)
                 (lambda (command &rest _) (push (cons default-directory command) calls))))
        (let ((default-directory (expand-file-name "src/" root)))
          (should (equal '("dev" "build") (bun/scripts)))
          (bun/run-script "dev")
          (bun/test "it works")
          (bun/test nil)
          (bun/install)
          (bun/add "zod hono" t)
          (bun/x "cowsay hi")
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "src/a.ts" root))
            (bun/test-file)
            (setq buffer-file-name nil))))
      (should (cl-every (lambda (call) (equal root (car call))) calls))
      (should (equal (mapcar #'cdr (reverse calls))
                     (list "/s/bun run dev"
                           "/s/bun test it\\ works"
                           "/s/bun test"
                           "/s/bun install"
                           "/s/bun add --dev zod hono"
                           "/s/bun x cowsay hi"
                           (concat "/s/bun test " (shell-quote-argument
                                                   (expand-file-name "src/a.ts" root)))))))))

(ert-deftest bun/test-commands-outside-project ()
  (bun--test-with-tree '(("package.json" . "{}"))
    (let ((default-directory root))
      (should-error (bun/install) :type 'user-error))))

(ert-deftest bun/test-keymap ()
  (dolist (mode bun/modes)
    (should (eq bun/command-map
                (keymap-lookup (symbol-value (intern (format "%s-map" mode))) "C-c C-b")))))

;;; bun-mode-test.el ends here
