;;; jsonl-mode.el --- Major mode for JSON Lines (JSONL / NDJSON)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Grigory Shepelev

;; Author: Grigory Shepelev <mail@grigory.tech>
;; Keywords: languages, data, json
;; URL: https://github.com/shegeley/emacs-jsonl-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (jq-mode "0.5.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing JSON Lines files (a.k.a. JSONL or NDJSON):
;; a text format where each line is an independent, complete JSON value.
;;
;; `jsonl-mode' is derived from the built-in `js-json-mode', so it inherits
;; its font-lock, syntax table and indentation for free.  On top of that it
;; adds semantics that are specific to the one-record-per-line format:
;;
;; - navigation by record (`jsonl-forward-record' / `jsonl-backward-record'),
;; - counting records (`jsonl-count-records'),
;; - viewing a single record pretty-printed in a side buffer
;;   (`jsonl-pretty-print-record'), since records must stay on one line and
;;   cannot be reflowed in place,
;; - per-line validation via a Flymake backend (`jsonl-flymake') and a
;;   one-shot check (`jsonl-validate-buffer'),
;; - filtering records through jq: a one-shot `jsonl-jq', and a live-preview
;;   `jsonl-jq-interactively' (a thin wrapper around `jq-mode'), both with -c
;;   passed by default so the JSON Lines shape is preserved.
;;
;; Files ending in .jsonl or .ndjson are handled automatically.
;;
;; jq filtering is built on the `jq-mode' package: `jsonl-jq-interactively'
;; calls its `jq-interactively', and in `jsonl-mode' buffers jq-mode is
;; pointed at the same jq binary as `jsonl-jq' and defaulted to -c.
;;
;; Example configuration:
;;
;;   (use-package jsonl-mode
;;     :ensure t)

;;; Code:

(require 'js)                           ; `js-json-mode'
(require 'json)                         ; `json-parse-string', pretty printer
(require 'flymake)
(require 'subr-x)                       ; `string-blank-p'
(require 'jq-mode)                      ; `jq-interactively', live-preview jq

(defgroup jsonl nil
  "Editing JSON Lines (JSONL / NDJSON) files."
  :group 'languages
  :prefix "jsonl-")

(defcustom jsonl-mode-enable-flymake t
  "When non-nil, enable Flymake in `jsonl-mode' buffers.
Each non-blank line is validated as an independent JSON value."
  :type 'boolean
  :group 'jsonl)

(defcustom jsonl-jq-command "jq"
  "Program used by `jsonl-jq' to filter records.
Defaults to \"jq\", found on `exec-path'.  The x-files Guix package
rewrites this default to the absolute jq store path at build time."
  :type 'string
  :group 'jsonl)

(defcustom jsonl-jq-default-flags '("-c")
  "Flags always passed to `jsonl-jq-command' by `jsonl-jq'.
The default \"-c\" keeps jq output as one JSON value per line, preserving
the JSON Lines shape.  Drop it to get jq's pretty-printed output instead."
  :type '(repeat string)
  :group 'jsonl)

;;;; Record helpers

(defun jsonl--current-line ()
  "Return the current line as a string, without text properties."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

(defun jsonl--map-records (fn)
  "Call FN with (BEG END LINENO) for every non-blank record in the buffer.
BEG and END delimit the record's line; LINENO is its 1-based line number.
Blank lines are skipped, matching JSONL semantics."
  (save-excursion
    (goto-char (point-min))
    (let ((lineno 1))
      (while (not (eobp))
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (unless (string-blank-p (buffer-substring-no-properties beg end))
            (funcall fn beg end lineno)))
        (setq lineno (1+ lineno))
        (forward-line 1)))))

(defun jsonl--collect-errors ()
  "Return a list of parse errors for the records in the buffer.
Each element is a plist with :beg, :end, :lineno and :message keys."
  (let (errs)
    (jsonl--map-records
     (lambda (beg end lineno)
       (condition-case err
           (json-parse-string (buffer-substring-no-properties beg end))
         (json-error
          (push (list :beg beg :end end :lineno lineno
                      :message (error-message-string err))
                errs)))))
    (nreverse errs)))

;;;; Interactive commands

(defun jsonl-forward-record (&optional n)
  "Move point to the beginning of the Nth next non-blank record.
With a negative N, move backward.  N defaults to 1."
  (interactive "p")
  (setq n (or n 1))
  (let ((dir (if (< n 0) -1 1)))
    (forward-line 0)
    (dotimes (_ (abs n))
      (forward-line dir)
      (while (and (not (bobp)) (not (eobp))
                  (string-blank-p (jsonl--current-line)))
        (forward-line dir)))))

(defun jsonl-backward-record (&optional n)
  "Move point to the beginning of the Nth previous non-blank record.
N defaults to 1."
  (interactive "p")
  (jsonl-forward-record (- (or n 1))))

(defun jsonl-count-records ()
  "Report the number of non-blank records in the buffer.
Return that count."
  (interactive)
  (let ((n 0))
    (jsonl--map-records (lambda (_beg _end _lineno) (setq n (1+ n))))
    (when (called-interactively-p 'interactive)
      (message "jsonl: %d record%s" n (if (= n 1) "" "s")))
    n))

(defun jsonl-pretty-print-record ()
  "Show the JSON record on the current line, pretty-printed, in another buffer.
Records must stay on a single line in the source buffer, so the expanded
form is rendered in a read-only \"*JSONL Record*\" buffer instead."
  (interactive)
  (let ((text (jsonl--current-line)))
    (when (string-blank-p text)
      (user-error "No JSON record on this line"))
    (condition-case err
        (json-parse-string text)
      (json-error
       (user-error "Invalid JSON on this line: %s" (error-message-string err))))
    (let ((buf (get-buffer-create "*JSONL Record*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert text)
          (json-pretty-print-buffer)
          (js-json-mode)
          (goto-char (point-min)))
        (view-mode 1))
      (display-buffer buf))))

(defun jsonl-validate-buffer ()
  "Validate every record in the buffer as an independent JSON value.
Report the count and line numbers of any invalid records."
  (interactive)
  (let ((errs (jsonl--collect-errors)))
    (if errs
        (message "jsonl: %d invalid record%s on line%s %s"
                 (length errs)
                 (if (= (length errs) 1) "" "s")
                 (if (= (length errs) 1) "" "s")
                 (mapconcat (lambda (e) (number-to-string (plist-get e :lineno)))
                            errs ", "))
      (message "jsonl: all records are valid JSON"))))

;;;; jq integration

(defvar jsonl-jq-filter-history nil
  "Minibuffer history of jq filters used by `jsonl-jq'.")

(defun jsonl-jq (filter &optional beg end)
  "Filter the buffer's records through jq using FILTER, show the result.
The text between BEG and END is streamed through `jsonl-jq-command' with
`jsonl-jq-default-flags' (\"-c\" by default, so the output stays one JSON
value per line).  BEG and END default to the active region, or the whole
buffer when there is none.  Output goes to a read-only \"*JSONL jq*\"
buffer in `jsonl-mode'; on failure jq's stderr is shown there instead."
  (interactive
   (list (read-string "jq filter: " (car jsonl-jq-filter-history)
                      'jsonl-jq-filter-history)
         (and (use-region-p) (region-beginning))
         (and (use-region-p) (region-end))))
  (unless (executable-find jsonl-jq-command)
    (user-error "jq executable not found: %s" jsonl-jq-command))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (buf (get-buffer-create "*JSONL jq*"))
         (errfile (make-temp-file "jsonl-jq"))
         status)
    (with-current-buffer buf
      ;; A prior run leaves the buffer read-only via `view-mode'; clear that
      ;; so jq's output can be written into it.
      (when (bound-and-true-p view-mode) (view-mode -1))
      (setq buffer-read-only nil)
      (erase-buffer))
    (unwind-protect
        (progn
          (setq status
                (apply #'call-process-region beg end jsonl-jq-command
                       nil (list buf errfile) nil
                       (append jsonl-jq-default-flags (list filter))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (unless (and (integerp status) (zerop status))
                (goto-char (point-max))
                (insert-file-contents errfile))
              (goto-char (point-min))
              (if (and (integerp status) (zerop status))
                  (jsonl-mode)
                (fundamental-mode)))
            (view-mode 1)))
      (delete-file errfile))
    (display-buffer buf)
    (unless (and (integerp status) (zerop status))
      (message "jsonl-jq: jq exited with status %s" status))
    buf))

(defun jsonl-jq-interactively (beg end)
  "Filter records through jq with a live preview, via `jq-mode'.
Operates on the active region, or the whole buffer when there is none.
Inside `jsonl-mode' the preview uses the same jq binary as `jsonl-jq' and
defaults to -c, so the JSON Lines shape is preserved (see
`jsonl-jq-default-flags' and `jq-interactive-default-options')."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (jq-interactively beg end))

;;;; Flymake

(defun jsonl-flymake (report-fn &rest _args)
  "A Flymake backend validating each JSONL record.
REPORT-FN is called with one diagnostic per invalid record."
  (funcall report-fn
           (mapcar (lambda (e)
                     (flymake-make-diagnostic
                      (current-buffer)
                      (plist-get e :beg) (plist-get e :end)
                      :error (plist-get e :message)))
                   (jsonl--collect-errors))))

;;;; Mode

(defvar-keymap jsonl-mode-map
  :doc "Keymap for `jsonl-mode'."
  "C-c C-f" #'jsonl-forward-record
  "C-c C-b" #'jsonl-backward-record
  "C-c C-p" #'jsonl-pretty-print-record
  "C-c C-v" #'jsonl-validate-buffer
  "C-c C-l" #'jsonl-count-records
  "C-c C-j" #'jsonl-jq
  "C-c C-i" #'jsonl-jq-interactively)

;;;###autoload
(define-derived-mode jsonl-mode js-json-mode "JSONL"
  "Major mode for editing JSON Lines (JSONL / NDJSON) files.

Each line is an independent, complete JSON value.  Derived from
`js-json-mode', so JSON font-lock and indentation come for free; on top
of that this mode adds record navigation, per-line validation,
pretty-printing and jq filtering.

\\{jsonl-mode-map}"
  ;; One record per line: reflowing would corrupt the format.
  (setq-local truncate-lines t)
  (auto-fill-mode -1)
  ;; Route jq-mode's live preview through the same (possibly store-baked) jq
  ;; binary as `jsonl-jq', and keep -c so it preserves the JSON Lines shape.
  (setq-local jq-interactive-command jsonl-jq-command)
  (setq-local jq-interactive-default-options
              (string-join jsonl-jq-default-flags " "))
  (add-hook 'flymake-diagnostic-functions #'jsonl-flymake nil t)
  (when jsonl-mode-enable-flymake
    (flymake-mode 1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jsonl\\'" . jsonl-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ndjson\\'" . jsonl-mode))

(provide 'jsonl-mode)
;;; jsonl-mode.el ends here
