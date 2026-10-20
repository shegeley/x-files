(define-module (x-files packages emacs macher)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix git-download) #:select (git-fetch git-reference
                                               git-file-name git-version))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-gptel
                                                  emacs-markdown-mode))
  #:use-module ((gnu packages emacs-build) #:select (emacs-buttercup))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((gnu packages rsync) #:select (rsync))
  #:use-module ((gnu packages base) #:select (diffutils))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-xyz) #:select (python-jsonschema))
  #:use-module ((gnu packages bash) #:select (bash-minimal)))

(define (test-file name)
  (local-file (search-path %load-path
                           (string-append "x-files/packages/aux/" name))))

(define-public emacs-macher
  (let ((commit "66fac138a09f02edc0750218180db65ec3e25b1f"))
    (package
      (name "emacs-macher")
      (version (git-version "0.5.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kmontag/macher")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10xiywz455166p1hiycv4qbiraldvzf8ybnny5lwn3vfzsszprc9"))))
      (build-system emacs-build-system)
      (arguments
       (list
        ;; Functional tests need an authenticated model.  Unit and mocked
        ;; integration tests run offline in the build sandbox.
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'check 'offline-schema-validator
              (lambda _
                ;; Upstream invokes npx to download a JSON Schema validator.
                (substitute* "tests/test-integration.el"
                  (("\"npx\"") "\"python3\"")
                  (("\"jsonschema\"")
                   (string-append
                    "\"" #$(test-file "macher-validate-schema.py")
                    "\""))
                  ;; Typo in upstream's after-all cleanup.
                  (("original-debugon-quit") "original-debug-on-quit"))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  ;; TRAMP's mock shell needs a writable home and store PATH.
                  (setenv "HOME" (getcwd))
                  ;; The suites assume a fresh Emacs, including no file buffers.
                  (for-each
                   (lambda (suite)
                     (invoke "emacs" "--batch" "-L" "." "-L" "tests"
                             "-l" #$(test-file "macher-test-init.el")
                             "-l" "buttercup" "-l" "tests/test-setup.el"
                             "-l" suite "-f" "buttercup-run"))
                   '("tests/test-unit.el" "tests/test-integration.el"))))))))
      (native-inputs (list emacs-buttercup git-minimal python python-jsonschema))
      (propagated-inputs (list emacs-gptel diffutils))
      (home-page "https://github.com/kmontag/macher")
      (synopsis "Project-aware edits with reviewable patches in Emacs")
      (description
       "Macher extends gptel with project context and file-editing tools.
Changes are staged in memory and presented as patches for review.")
      (license license:gpl3+))))

(define-public emacs-macher-agent
  (let ((commit "f60786359a175d6c7f0c56ec3faeb74ae2bda22d"))
    (package
      (name "emacs-macher-agent")
      (version (git-version "0.8.3.12" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/elij/macher-agent")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mwafim44gk22zl72qlykn1dhggvc4bsljmdjmds2s2r113czbky"))
         (patches
          (list (search-path
                 %load-path
                 "x-files/packages/aux/macher-agent-context-isolation.patch")))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:include #~(cons "^skills/" %default-include)
        #:test-command
        #~(list "emacs" "--batch" "-L" "." "-L" "tests"
                "--eval" "(setq print-length 30 print-level 8)"
                "-l" "buttercup" "-f" "buttercup-run-discover" "tests")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'unpack-tests
              (lambda* (#:key inputs #:allow-other-keys)
                (copy-recursively (assoc-ref inputs "test-source") "tests")))
            (add-after 'unpack 'load-markdown-mode
              (lambda _
                ;; Child buffers need a text major mode before gptel-mode.
                (substitute* "macher-agent-orchestration.el"
                  (("\\(require 'subr-x\\)")
                   "(require 'subr-x)\n(require 'markdown-mode)"))
                ;; These are parsed as tool ASTs, not loaded as libraries.
                (substitute* (find-files "skills/scripts" "\\.el$")
                  (("lexical-binding: t;")
                   "lexical-binding: t; no-byte-compile: t;"))))
            (add-before 'check 'test-home
              (lambda _
                (mkdir-p "test-home")
                (setenv "HOME" (string-append (getcwd) "/test-home"))))
            (add-after 'install 'installed-smoke-test
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (setenv "HOME" (getcwd))
                  (setenv "MACHER_TEST_PACKAGE" #$output)
                  (invoke "emacs" "--batch"
                          "-L" (dirname
                                (car (find-files #$output "^macher-agent\\.el$")))
                          "--eval" "(setq print-length 30 print-level 8)"
                          "-l" #$(test-file "macher-agent-smoke-test.el"))))))))
      (native-inputs
       (list (list "emacs-buttercup" emacs-buttercup)
             (list "test-source"
                   (origin
                     (method git-fetch)
                     (uri (git-reference
                           (url "https://github.com/elij/macher-agent-tests")
                           (commit "1647dda198ba27154fd6265f1713b229bc47a338")))
                     (file-name "macher-agent-tests")
                     (sha256
                      (base32 "0pc5m1hancy4p2jnfp0y883rhy2zkvibn4808f1nf1km0lx9jq0f"))))))
      (propagated-inputs
       (list emacs-macher emacs-gptel emacs-markdown-mode
             git-minimal rsync bash-minimal))
      (home-page "https://github.com/elij/macher-agent")
      (synopsis "Emacs agent orchestration with isolated editing contexts")
      (description
       "Macher Agent provides asynchronous subagents, staged edits, context
merging and conversation memory within Emacs.  It uses gptel for model access
and Macher for project editing.")
      (license license:gpl3+))))
