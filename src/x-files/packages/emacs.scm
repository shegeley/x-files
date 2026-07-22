(define-module (x-files packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((gnu packages emacs) #:select (emacs))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-gptel
                                                   emacs-org-ql
                                                   emacs-tramp))
  #:use-module ((gnu packages admin) #:select (ansible-core))
  #:use-module ((gnu packages tree-sitter) #:select (tree-sitter-prisma))
  #:use-module (gnu packages))

(define-public emacs-prisma-ts-mode
  (let [(url "https://github.com/nverno/prisma-ts-mode")
        (commit "c63117764dc9e177aea7ddbef23c47feba1523d8")
        (version "1.0.0")
        (hash "19mhcr7v11shhzf1p5b6wbb9b8hx1q0wpzwrxhgypmzvx9837mgz")]
    (package
      (name "emacs-prisma-ts-mode")
      (version (git-version version "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url url)
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs (list tree-sitter-prisma))
      (home-page url)
      (synopsis "Major mode for Prisma using tree-sitter")
      (description "Tree-sitter based major mode for Prisma schema files.
Provides support for indentation, font-locking, imenu, and structural navigation.")
      (license license:gpl3+))))

(define-public emacs-gptel-got
  (let [(url "https://codeberg.org/bajsicki/gptel-got")
        (commit "f25c56c4b3fae3bf435e3a82723438b49ac6878b")
        (version "0.0.2")
        (hash "1n0gpzi1liz38xxy2l3cp2jwcw5j2qf1vfz5i29pvhvffqxsz4kh")]
    (package
      (name "emacs-gptel-got")
      (version (git-version version "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url url)
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ;; no tests
      (propagated-inputs (list emacs-gptel emacs-org-ql))
      (home-page url)
      (synopsis "LLM Tools for org-mode interaction")
      (description "This is a package which expands the functionality of gptel for interacting with org-mode")
      (license license:gpl3+))))

(define-public emacs-ansible-vault
  (let [(url "https://github.com/freehck/ansible-vault-mode")
        (commit "74f96ce226f51bec203af343f73182ea132749a6")
        (version "0.6.1")
        (hash "1mj07znw8chdxcg0hi07mgay0i9p55sy8fwaw1waxfp47izqvq1x")]
    (package
      (name "emacs-ansible-vault")
      (version (git-version version "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url url)
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:tests? #f ;; no tests
        #:phases
        #~(modify-phases %standard-phases
            ;; The mode's only external call is the `ansible-vault' CLI, taken
            ;; from a `defcustom' defaulting to the bare name on PATH.  Bake in
            ;; the absolute store path so it works without extra profile setup.
            (add-after 'unpack 'hardcode-ansible-vault-command
              (lambda _
                (substitute* "ansible-vault.el"
                  (("\"ansible-vault\"")
                   (string-append
                    "\"" #$(file-append ansible-core "/bin/ansible-vault")
                    "\""))))))))
      ;; ansible-vault ships in ansible-core; propagate it so the CLI is present
      ;; in the profile as well (the hardcoded path already pins the closure).
      (propagated-inputs (list ansible-core))
      (home-page url)
      (synopsis "Minor mode for editing ansible-vault encrypted files")
      (description "This package provides @code{ansible-vault-mode}, an Emacs
minor mode for transparently editing files encrypted with Ansible Vault.  On
opening an encrypted file it decrypts the buffer with the
@command{ansible-vault} command and re-encrypts it on save, so the plaintext of
a secret never touches disk.")
      (license license:gpl3+))))

(define-public emacs-tramp-rpc
  (package
    (name "emacs-tramp-rpc")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ArthurHeymans/emacs-tramp-rpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yiwa7hsc8anj1l6902ycwmcxjwkl4crrn7h84797m2dbrh524gm"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-tramp))
    (arguments
     (list
      #:emacs emacs
      #:lisp-directory "lisp"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'make-autoloads 'defer-autoloads
            ;; The ;;;###autoload'd `add-to-list' / `tramp-register-foreign-
            ;; file-name-handler' forms call into tramp; the emacs-build-system
            ;; now loads the compiled autoloads in isolation to validate them,
            ;; which fails with a void function.  Wrap each in `eval-after-load'
            ;; so they defer until tramp-rpc is loaded.  Done via emacs `read'
            ;; so it is robust to the forms being pretty-printed across lines.
            (lambda _
              (invoke
               "emacs" "-Q" "--batch" "--eval"
               "(let ((f \"tramp-rpc-autoloads.el\"))
                  (with-temp-buffer
                    (insert-file-contents f)
                    (goto-char (point-min))
                    (while (re-search-forward
                            \"^(\\\\(tramp-register-foreign-file-name-handler\\\\|add-to-list \\\\)\"
                            nil t)
                      (goto-char (match-beginning 0))
                      (let ((beg (point)) (form (read (current-buffer))))
                        (delete-region beg (point))
                        (goto-char beg)
                        (insert (format \"(eval-after-load 'tramp-rpc '%S)\" form))))
                    (write-region (point-min) (point-max) f nil 'silent)))"))))))
    (home-page "https://github.com/ArthurHeymans/emacs-tramp-rpc")
    (synopsis "High-performance TRAMP frontend using JSON-RPC")
    (description
     "A high-performance TRAMP backend for Emacs that uses a binary RPC server
instead of parsing shell command output.")
    (license license:gpl3+)))
