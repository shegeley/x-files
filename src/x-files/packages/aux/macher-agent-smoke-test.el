;;; macher-agent-smoke-test.el --- Installed package checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'macher-agent)

(ert-deftest macher-package-starts-and-reenables-project-session ()
  (let* ((root (make-temp-file "macher-session-" t))
         (default-directory (file-name-as-directory root))
         child)
    (unwind-protect
        (progn
          (should (= 0 (call-process "git" nil nil nil "init" "-q")))
          (macher-agent-install)
          (with-temp-buffer
            (text-mode)
            (gptel-mode 1)
            (macher-agent-mode 1)
            (should (macher-agent-valid-context-p macher-agent--persistent-context))
            (setq child (macher-agent-add-subagent
                         "*macher-package-child*" nil (current-buffer)))
            (should (buffer-live-p child))
            (with-current-buffer child
              (should (derived-mode-p 'markdown-mode))
              (should gptel-mode)
              (should (macher-agent-valid-context-p macher-agent--persistent-context)))
            (macher-agent-mode -1)
            (should-not macher-agent-mode)
            (macher-agent-mode 1)
            (should macher-agent-mode)))
      (when (buffer-live-p child) (kill-buffer child))
      (delete-directory root t))))

(ert-deftest macher-package-loads-bundled-tools ()
  (should (file-in-directory-p (symbol-file 'macher-agent-mode)
                              (getenv "MACHER_TEST_PACKAGE")))
  (let ((context (macher-agent--make-context)))
    (macher-agent-initialize-skills context)
    (dolist (name '("spawn_subagent" "execute_subagents" "submit_task_result"
                    "read_buffer_in_workspace" "write_buffer_in_workspace"))
      (should (macher-agent-resolve-tool
               name (macher-agent-workspace-tools-registry context) nil context)))))

(ert-deftest macher-package-child-edits-remain-staged ()
  (let* ((root (file-name-as-directory (make-temp-file "macher-smoke-" t)))
         (default-directory root)
         (file (expand-file-name "hello.txt" root))
         (parent (macher-agent--make-context :project-root root))
         child patch)
    (unwind-protect
        (progn
          (with-temp-file file (insert "original\n"))
          (macher-agent-context-update parent file "parent\n")
          (setq child (macher-agent--clone-context parent))
          (macher-agent-context-update child file "child\n")
          (should (equal "parent\n" (macher-agent-context-read parent file)))
          (macher-agent--merge-contexts parent child)
          (should (equal "child\n" (macher-agent-context-read parent file)))
          (should (equal "original\n"
                         (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (setq patch (macher-agent-macher-build-patch parent "Package smoke test"))
          (should (buffer-live-p patch))
          (with-current-buffer patch
            (should (string-match-p "^+child$" (buffer-string)))))
      (when (buffer-live-p patch) (kill-buffer patch))
      (delete-directory root t))))

(ert-deftest macher-package-copies-workspace-with-rsync ()
  (let* ((root (make-temp-file "macher-rsync-" t))
         (target (make-temp-file "macher-sandbox-" t))
         (default-directory (file-name-as-directory root))
         (shell-file-name (executable-find "bash")))
    (unwind-protect
        (progn
          (should (= 0 (call-process "git" nil nil nil "init" "-q")))
          (with-temp-file "tracked.txt" (insert "tracked\n"))
          (with-temp-file "untracked.txt" (insert "untracked\n"))
          (with-temp-file ".gitignore" (insert "ignored.txt\n"))
          (with-temp-file "ignored.txt" (insert "ignored\n"))
          (should (= 0 (call-process "git" nil nil nil "add" "tracked.txt" ".gitignore")))
          (should (= 0 (macher-agent--vfs-sync-baseline root target)))
          (should (file-exists-p (expand-file-name "tracked.txt" target)))
          (should (file-exists-p (expand-file-name "untracked.txt" target)))
          (should-not (file-exists-p (expand-file-name "ignored.txt" target))))
      (delete-directory root t)
      (delete-directory target t))))

(ert-run-tests-batch-and-exit "^macher-package-")

;;; macher-agent-smoke-test.el ends here
