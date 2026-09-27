;;; macher-test-init.el --- Guix build environment for Macher tests -*- lexical-binding: t; -*-

(require 'tramp)

(setq print-length 30
      print-level 8
      tramp-remote-path (split-string (getenv "PATH") path-separator t))

;; Upstream's mock TRAMP method runs locally inside the build sandbox.
(connection-local-set-profile-variables
 'macher-guix-test
 `((shell-file-name . ,(executable-find "sh"))
   (shell-command-switch . "-c")))
(connection-local-set-profiles
 '(:application tramp :protocol "mock") 'macher-guix-test)

;;; macher-test-init.el ends here
