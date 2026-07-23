;;; jsonl-mode-test.el --- Tests for jsonl-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the pure record helpers of `jsonl-mode'.

;;; Code:

(require 'ert)
(require 'jsonl-mode)

(defmacro jsonl-test--with-buffer (content &rest body)
  "Insert CONTENT into a temp buffer and evaluate BODY there."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     ,@body))

(ert-deftest jsonl-test-count-records ()
  "Non-blank lines are counted; blank lines are ignored."
  (jsonl-test--with-buffer "{\"a\":1}\n{\"b\":2}\n\n{\"c\":3}\n"
    (should (= (jsonl-count-records) 3))))

(ert-deftest jsonl-test-collect-errors-all-valid ()
  "Objects, arrays, strings and numbers are all valid records."
  (jsonl-test--with-buffer "{\"a\":1}\n[1,2,3]\n\"str\"\n42\n"
    (should (null (jsonl--collect-errors)))))

(ert-deftest jsonl-test-collect-errors-reports-line ()
  "An invalid record is reported with its 1-based line number."
  (jsonl-test--with-buffer "{\"a\":1}\n{bad}\n{\"c\":3}\n"
    (let ((errs (jsonl--collect-errors)))
      (should (= (length errs) 1))
      (should (= (plist-get (car errs) :lineno) 2)))))

(ert-deftest jsonl-test-forward-record-skips-blanks ()
  "`jsonl-forward-record' lands on the next non-blank record."
  (jsonl-test--with-buffer "{\"a\":1}\n\n\n{\"b\":2}\n"
    (goto-char (point-min))
    (jsonl-forward-record 1)
    (should (equal (jsonl--current-line) "{\"b\":2}"))))

(ert-deftest jsonl-test-jq-preserves-jsonl ()
  "`jsonl-jq' filters records and keeps one JSON value per line."
  (skip-unless (executable-find jsonl-jq-command))
  (jsonl-test--with-buffer "{\"n\":1}\n{\"n\":2}\n{\"n\":3}\n"
    (jsonl-jq ".n"))
  (should (equal (with-current-buffer "*JSONL jq*"
                   (buffer-substring-no-properties (point-min) (point-max)))
                 "1\n2\n3\n")))

(provide 'jsonl-mode-test)
;;; jsonl-mode-test.el ends here
