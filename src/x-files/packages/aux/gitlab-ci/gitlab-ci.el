;;; gitlab-ci.el --- lab.el multi-host token wiring -*- lexical-binding: t -*-

;;; Commentary:

;; Reads a per-host GitLab API token from a plain file and sets
;; `lab-config' (see lab.el) so `lab-watch-pipeline-for-last-commit',
;; `lab-list-project-pipelines', `lab-show-job-logs' etc. authenticate
;; without prompting.  A host with no readable token file is still added
;; to `lab-config' (without :token), so lab.el falls back to `auth-source'
;; for it instead.

;;; Code:

(defgroup gitlab-ci nil
  "Per-host token wiring for lab.el."
  :group 'lab)

(defcustom gitlab-ci-hosts
  '("https://gitlab.com")
  "GitLab hosts to authenticate lab.el against."
  :type '(repeat string)
  :group 'gitlab-ci)

(defcustom gitlab-ci-token-directory
  (expand-file-name "tokens" (or (getenv "STORAGE")
                                 (expand-file-name "g-files/storage" "~")))
  "Directory holding per-host token files, each named HOST.token."
  :type 'string
  :group 'gitlab-ci)

(defun gitlab-ci--read-token (host)
  "Return the token for HOST from its `gitlab-ci-token-directory' file, or nil."
  (let* ((bare-host (replace-regexp-in-string "\\`https?://" "" host))
         (file (expand-file-name (concat bare-host ".token")
                                 gitlab-ci-token-directory)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string))))))

(with-eval-after-load 'lab
  (setq lab-config
        (mapcar (lambda (host)
                  (let ((token (gitlab-ci--read-token host)))
                    (if token
                        (list :host host :token token)
                        (list :host host))))
                gitlab-ci-hosts)))

(provide 'gitlab-ci)
;;; gitlab-ci.el ends here
