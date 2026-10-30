;;; bun-mode.el --- Major modes, LSP and commands for Bun projects -*- lexical-binding: t -*-

;;; Commentary:

;; Bun ships no language server, so `bun/*-mode' buffers get one eglot
;; server that is really two: rass (rassumfrassum) multiplexes the native
;; TypeScript 7 server (`tsc --lsp': types, completion, navigation) and
;; Biome (`biome lsp-proxy': lint, format, fixes).  `bun-rass.py' makes
;; formatting go to Biome.
;;
;; A file gets a `bun/*-mode' only inside a Bun project (see
;; `bun/project-markers'); otherwise `bun/mode-maybe' defers to the next
;; `auto-mode-alist' entry, so deno-mode and plain TypeScript setups keep
;; working.  Executable paths are patched to Guix store paths at build time.

;;; Code:

(require 'seq)
(require 'json)
(require 'compile)
(require 'typescript-ts-mode)
(require 'js)

(defgroup bun nil
  "Bun JavaScript/TypeScript projects."
  :group 'languages)

(defcustom bun/bin "bun"
  "Absolute path to the bun executable."
  :type 'string)

(defcustom bun/tsc-bin "tsc"
  "Absolute path to the native TypeScript (7+) executable."
  :type 'string)

(defcustom bun/biome-bin "biome"
  "Absolute path to the biome executable."
  :type 'string)

(defcustom bun/rass-bin "rass"
  "Absolute path to the rass LSP multiplexer."
  :type 'string)

(defcustom bun/rass-log-level "warn"
  "The `--log-level' of rass, shown in the eglot events/stderr buffers."
  :type '(choice (const "silent") (const "warn") (const "info")
                 (const "event") (const "debug") (const "trace")))

(defcustom bun/project-markers '("bun.lock" "bun.lockb" "bunfig.toml")
  "Files whose presence in a directory makes it a Bun project root."
  :type '(repeat string))

(defconst bun/rass-preset
  (expand-file-name "bun-rass.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The rass preset: Biome formats, TypeScript answers the rest.")

(defconst bun/extension->mode
  '(("ts"  . bun/ts-mode)
    ("mts" . bun/ts-mode)
    ("cts" . bun/ts-mode)
    ("tsx" . bun/tsx-mode)
    ("js"  . bun/js-mode)
    ("mjs" . bun/js-mode)
    ("cjs" . bun/js-mode)
    ("jsx" . bun/jsx-mode))
  "File extension to `bun/*-mode'.")

(defconst bun/auto-mode-regexps
  '("\\.[cm]?ts\\'" "\\.tsx\\'" "\\.[cm]?js\\'" "\\.jsx\\'")
  "`auto-mode-alist' keys handled by `bun/mode-maybe'.")

(defun bun/project-root (&optional directory)
  "Nearest Bun project root at or above DIRECTORY, or nil."
  (when-let* ((directory (or directory default-directory)))
    (locate-dominating-file
     directory
     (lambda (dir)
       (seq-some (lambda (marker) (file-exists-p (expand-file-name marker dir)))
                 bun/project-markers)))))

(defun bun--root ()
  "Current Bun project root, or a `user-error'."
  (or (bun/project-root)
      (user-error "Not inside a Bun project (no %s)"
                  (string-join bun/project-markers ", "))))

;;;; Modes

(defvar-keymap bun/command-map
  :doc "Bun commands, bound to \\`C-c C-b' in `bun/*-mode' buffers."
  "r" #'bun/run-script
  "f" #'bun/run-file
  "t" #'bun/test
  "T" #'bun/test-file
  "i" #'bun/install
  "a" #'bun/add
  "x" #'bun/x)

(defmacro bun--define-mode (name parent lighter)
  "Define NAME as a `bun' flavour of PARENT showing LIGHTER."
  `(progn
     (define-derived-mode ,name ,parent ,lighter
       ,(format "`%s' inside a Bun project.\n\n\\{%s-map}" parent name))
     (keymap-set ,(intern (format "%s-map" name)) "C-c C-b" bun/command-map)))

(bun--define-mode bun/ts-mode  typescript-ts-mode "Bun/TS")
(bun--define-mode bun/tsx-mode tsx-ts-mode        "Bun/TSX")
(bun--define-mode bun/js-mode  js-ts-mode         "Bun/JS")
(bun--define-mode bun/jsx-mode js-ts-mode         "Bun/JSX")

(defconst bun/modes '(bun/ts-mode bun/tsx-mode bun/js-mode bun/jsx-mode))

(defun bun--fallback-mode ()
  "Set the major mode `auto-mode-alist' would pick without `bun/mode-maybe'."
  (let ((auto-mode-alist (seq-remove (lambda (entry) (eq (cdr entry) 'bun/mode-maybe))
                                     auto-mode-alist)))
    (set-auto-mode)))

;;;###autoload
(defun bun/mode-maybe ()
  "Use a `bun/*-mode' inside a Bun project, otherwise the usual mode."
  (if-let* ((file (buffer-file-name))
            (mode (assoc-default (file-name-extension file) bun/extension->mode))
            ((bun/project-root (file-name-directory file))))
      (funcall mode)
    (bun--fallback-mode)))

(defun bun/install-auto-mode ()
  "Put `bun/mode-maybe' in front of `auto-mode-alist'.  Idempotent."
  (dolist (regexp (reverse bun/auto-mode-regexps))
    (setq auto-mode-alist
          (cons (cons regexp 'bun/mode-maybe)
                (seq-remove (lambda (entry) (equal entry (cons regexp 'bun/mode-maybe)))
                            auto-mode-alist)))))

(bun/install-auto-mode)
;; deno-mode prepends its own dispatcher when loaded; stay ahead of it.
(with-eval-after-load 'deno-mode (bun/install-auto-mode))

;;;; LSP

(defun bun/eglot-contact (&optional _interactive _project)
  "The eglot contact: rass over native TypeScript (primary) and Biome."
  (list bun/rass-bin "--log-level" bun/rass-log-level bun/rass-preset
        "--" bun/tsc-bin "--lsp" "--stdio"
        "--" bun/biome-bin "lsp-proxy"))

(defconst bun/eglot-modes
  '((bun/ts-mode  :language-id "typescript")
    (bun/tsx-mode :language-id "typescriptreact")
    (bun/js-mode  :language-id "javascript")
    (bun/jsx-mode :language-id "javascriptreact"))
  "The eglot mode entries; derived mode names would give bogus language ids.")

(defvar eglot-server-programs)

(defun bun/eglot-register ()
  "Put the bun entry in front of `eglot-server-programs'.  Idempotent.
The bun modes derive from the stock ones and eglot takes the first entry
whose mode matches, so the entry must precede generic TypeScript ones."
  (let ((entry (cons bun/eglot-modes #'bun/eglot-contact)))
    (setq eglot-server-programs
          (cons entry (remove entry eglot-server-programs)))))

(with-eval-after-load 'eglot (bun/eglot-register))

;;;; Commands

(defun bun--compile (command)
  "Run bun COMMAND (a list of arguments) from the project root."
  (let ((default-directory (bun--root)))
    (compile (mapconcat #'shell-quote-argument (cons bun/bin command) " "))))

(defun bun/scripts (&optional root)
  "Script names from package.json at ROOT (default: project root)."
  (let ((file (expand-file-name "package.json" (or root (bun--root)))))
    (when (file-exists-p file)
      (mapcar (lambda (script) (symbol-name (car script)))
              (alist-get 'scripts (json-read-file file))))))

(defun bun/run-script (script)
  "Run package.json SCRIPT with `bun run'."
  (interactive (list (completing-read "bun run: " (bun/scripts) nil t)))
  (bun--compile (list "run" script)))

(defun bun/run-file (file)
  "Run FILE (default: the current one) with `bun run'."
  (interactive (list (read-file-name "bun run file: " nil (buffer-file-name) t)))
  (bun--compile (list "run" (expand-file-name file))))

(defun bun/test (&optional filter)
  "Run `bun test', optionally only tests matching FILTER."
  (interactive (list (let ((s (read-string "bun test filter (empty: all): ")))
                       (unless (string-empty-p s) s))))
  (bun--compile (cons "test" (when filter (list filter)))))

(defun bun/test-file ()
  "Run `bun test' for the current file."
  (interactive)
  (bun--compile (list "test" (expand-file-name (buffer-file-name)))))

(defun bun/install ()
  "Run `bun install'."
  (interactive)
  (bun--compile (list "install")))

(defun bun/add (packages &optional dev)
  "`bun add' PACKAGES (space separated); with prefix arg DEV, as dev deps."
  (interactive (list (read-string "bun add: ") current-prefix-arg))
  (bun--compile (append '("add") (when dev '("--dev")) (split-string packages))))

(defun bun/x (command)
  "Run COMMAND (a package binary and its arguments) with `bun x'."
  (interactive (list (read-string "bunx: ")))
  (bun--compile (cons "x" (split-string-and-unquote command))))

(provide 'bun-mode)
;;; bun-mode.el ends here
