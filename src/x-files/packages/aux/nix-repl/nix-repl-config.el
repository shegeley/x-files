;;; nix-repl-config.el --- interactive Nix REPL commands -*- lexical-binding: t; -*-

;;; Commentary:

;; `nix-repl' (bundled with nix-mode, comint-based) only gives a raw process
;; buffer plus TAB-completion.  These commands add the eval-region/
;; eval-buffer convention every other REPL integration in this config uses.
;; No keybindings are assigned here -- bind these commands yourself if
;; wanted.

;;; Code:

(require 'nix-repl)
(require 'nix-prettify-mode)

(nix-prettify-global-mode 1)

(defun nix-repl-send-region (start end)
  "Send the buffer text between START and END to the Nix REPL.
Starts the REPL first if it isn't running yet."
  (interactive "r")
  (let ((text (string-trim (buffer-substring-no-properties start end))))
    (nix-repl-show)
    (with-current-buffer "*Nix-REPL*"
      (goto-char (point-max))
      (insert text)
      (comint-send-input))))

(defun nix-repl-send-line ()
  "Send the current line to the Nix REPL."
  (interactive)
  (nix-repl-send-region (line-beginning-position) (line-end-position)))

(defun nix-repl-send-buffer ()
  "Send the whole buffer to the Nix REPL."
  (interactive)
  (nix-repl-send-region (point-min) (point-max)))

(provide 'nix-repl-config)
;;; nix-repl-config.el ends here
