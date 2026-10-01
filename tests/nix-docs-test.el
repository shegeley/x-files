;;; nix-docs-test.el --- Offline documentation tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'nix-docs)

(ert-deftest nix-docs/test-decode-utf8-and-markup ()
  (let* ((record (concat
                  (mapconcat (lambda (text) (base64-encode-string (encode-coding-string text 'utf-8) t))
                             '("file:///manual.html" "Тест" "Use &lt;pkg&gt; &amp; overrideAttrs") " ")
                  " \n"))
         (hits (nix-docs/decode (concat "Recoll query: Query(x)\n1 results\n" record))))
    (should (= (length hits) 1))
    (should (equal (alist-get 'title (car hits)) "Тест"))
    (should (equal (alist-get 'excerpt (car hits)) "Use <pkg> & overrideAttrs"))))

(ert-deftest nix-docs/test-static-tree-paths ()
  (skip-unless (treesit-language-available-p 'nix))
  (dolist (example
           '(("{ services.multipath.enable = true; }" "enable" "services.multipath.enable")
             ("{ services = { multipath = { enable = true; }; }; }" "enable" "services.multipath.enable")
             ("{ x = pkgs.hello.overrideAttrs {}; }" "overrideAttrs" "pkgs.hello.overrideAttrs")
             ("{ x = lib.mkIf true {}; }" "mkIf" "lib.mkIf")
             ("{ x = builtins.map; }" "map" "builtins.map")
             ("{ x = \"enable\"; }" "enable" nil)
             ("{ # enable\n x = 1; }" "enable" nil)
             ("{ services.${name}.enable = true; }" "enable" nil)
             ("{ x = true; }" "true" nil)))
    (pcase-let ((`(,code ,needle ,expected) example))
      (with-temp-buffer
        (insert code)
        (treesit-parser-create 'nix)
        (goto-char (point-min))
        (search-forward needle)
        (backward-char 1)
        (should (equal (nix-docs/identifier-at-point) expected))))))

(ert-deftest nix-docs/test-eldoc-composition-and-disable ()
  (with-temp-buffer
    (let* ((old-strategy eldoc-documentation-strategy)
           (provider (lambda (_callback) "LSP documentation")))
      (setq-local eldoc-documentation-functions (list provider))
      (nix-docs/mode 1)
      (nix-docs/mode 1)
      (should (memq provider eldoc-documentation-functions))
      (should (= 1 (cl-count #'nix-docs/eldoc eldoc-documentation-functions)))
      (nix-docs/mode -1)
      (should (equal eldoc-documentation-strategy old-strategy))
      (should (equal eldoc-documentation-functions (list provider))))))

(ert-deftest nix-docs/test-stale-callback ()
  (with-temp-buffer
    (insert "overrideAttrs mkDerivation")
    (goto-char (point-min))
    (let ((nix-docs/mode t) callback delivered)
      (cl-letf (((symbol-function 'nix-docs/lookup)
                 (lambda (_query cb) (setq callback cb) nil)))
        (nix-docs/eldoc (lambda (&rest _) (setq delivered t))))
      (goto-char (point-max))
      (funcall callback nil nil)
      (should-not delivered))))

(ert-deftest nix-docs/test-error-is-not-cached ()
  (let ((nix-docs/cache (make-hash-table :test #'equal))
        (nix-docs/index-directory "/no-such-nix-docs-index") result)
    (nix-docs/lookup "x" (lambda (hits error) (setq result (list hits error))))
    (should (equal (car result) nil))
    (should (stringp (cadr result)))
    (should (= 0 (hash-table-count nix-docs/cache)))))

(defun nix-docs/test-index (callback)
  "Test the real index asynchronously, then call CALLBACK with (RESULTS ERROR).
Run separately from synchronous ERT in a live Emacs event loop."
  (clrhash nix-docs/cache)
  (cl-labels
      ((next (terms results)
         (if (not terms)
             (funcall callback (nreverse results) nil)
           (let* ((term (car terms)) (query (nix-docs/query term)) (start (float-time)))
             (nix-docs/lookup
              query
              (lambda (hits error)
                (condition-case failure
                    (progn
                      (should-not error)
                      (should hits)
                      (should (cl-some (lambda (hit)
                                         (string-match-p
                                          (regexp-quote (car (last (split-string term "\\."))))
                                          (alist-get 'excerpt hit))) hits))
                      (should (cl-every (lambda (hit)
                                         (not (string-match-p "nixos-25\\." (alist-get 'url hit)))) hits))
                      (let ((elapsed (* 1000 (- (float-time) start)))
                            (cached-start (float-time)) cached)
                        (cl-letf (((symbol-function 'make-process)
                                   (lambda (&rest _) (ert-fail "Cache started a process"))))
                          (nix-docs/lookup query (lambda (result err) (should-not err) (setq cached result))))
                        (should (equal cached hits))
                        (next (cdr terms)
                              (cons (list term :query-ms elapsed
                                          :cache-ms (* 1000 (- (float-time) cached-start)))
                                    results))))
                  (error (funcall callback nil (format "%s: %s" term (error-message-string failure)))))))))))
    (next '("overrideAttrs" "builtins.map" "services.multipath.enable") nil)))

;;; nix-docs-test.el ends here
