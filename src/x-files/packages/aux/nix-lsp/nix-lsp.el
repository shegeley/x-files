;;; nix-lsp.el --- nixd wiring for lsp-mode and eglot -*- lexical-binding: t -*-

;;; Commentary:

;; Points both LSP clients this config runs at nixd (evaluation-backed
;; Nix language server, reflecting the live nix-daemon feature-nix-dev
;; itself sets up) for nix-mode/nix-ts-mode buffers:
;;
;;   - lsp-mode: `lsp-nix-nixd-server-path' (lsp-mode's own built-in
;;     nix-mode/nix-ts-mode client, lsp-nix.el, defaults to the bare
;;     "nixd" command name -- overridden to the absolute store path so
;;     it never relies on $PATH).
;;   - eglot: its built-in nix-mode entry tries "nil"/"rnix-lsp"/"nixd"
;;     off PATH via `eglot-alternatives' -- neither of the first two is
;;     packaged here, and PATH lookup is fragile, so it's overridden to
;;     point straight at the store path.
;;
;; `nix-lsp-nixd-exe' is patched to an absolute Guix store path at
;; build time.

;;; Code:

(defcustom nix-lsp-nixd-exe "nixd"
  "Absolute path to nixd, the Nix language server.
Patched to the real Guix store path at build time."
  :type 'string)

(with-eval-after-load 'lsp-mode
  (setq lsp-nix-nixd-server-path nix-lsp-nixd-exe)
  (add-hook 'nix-mode-hook 'lsp))

(with-eval-after-load 'eglot
  (let ((nix-eglot-entry (cons 'nix-mode (list nix-lsp-nixd-exe))))
    (add-to-list 'eglot-server-programs nix-eglot-entry)))

(provide 'nix-lsp)
;;; nix-lsp.el ends here
