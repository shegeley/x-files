(define-module (x-files features python)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:use-module (rde features)
  #:use-module (rde features emacs)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module ((gnu packages python)      #:select (python))
  #:use-module ((gnu packages python-xyz)  #:select (python-lsp-server
                                                      python-debugpy))
  #:use-module ((gnu packages tree-sitter) #:select (tree-sitter-python))

  #:export (feature-python
            python-emacs-forms))

(define* (python-emacs-forms config python-exe pylsp-exe
                              #:key (dape-port 5678))
  "Build the list of Emacs Lisp configuration forms for Python.
PYTHON-EXE and PYLSP-EXE are file-append gexps (or plain strings in tests).
Includes:
  - eglot hooks always
  - lsp-mode hooks when the 'emacs-lsp feature value is present in CONFIG
  - dape configs when the 'emacs-dape feature value is present in CONFIG"
  (define dape-forms
    `((with-eval-after-load 'dape
        (let ((python-exe ,python-exe))
          (setq dape-configs
                (append
                 `((debugpy
                    modes (python-mode python-ts-mode)
                    command ,python-exe
                    command-args ["-m" "debugpy.adapter"
                                  "--host" "127.0.0.1"
                                  "--port" :autoport]
                    :request "launch"
                    :cwd dape-cwd
                    :program dape-buffer-default
                    :justMyCode nil)
                   (debugpy-attach
                    modes (python-mode python-ts-mode)
                    :request "attach"
                    :connect (:host "127.0.0.1" :port ,,dape-port)))
                 dape-configs))))))

  `((with-eval-after-load 'eglot
      (add-hook 'python-mode-hook    'eglot-ensure)
      (add-hook 'python-ts-mode-hook 'eglot-ensure)
      (add-to-list 'eglot-server-programs
                   `((python-mode python-ts-mode) . (,,pylsp-exe))))

    ,@(if (get-value 'emacs-lsp config #f)
        `((with-eval-after-load 'lsp-mode
            (add-hook 'python-mode-hook    'lsp-deferred)
            (add-hook 'python-ts-mode-hook 'lsp-deferred)
            (setq lsp-pylsp-server-command (list ,pylsp-exe))))
        '())

    ,@(if (get-value 'emacs-dape config #f) dape-forms '())))

(define* (feature-python
          #:key
          (python python)
          (python-lsp-server python-lsp-server)
          (python-debugpy python-debugpy)
          (dape-port 5678))

  "Setup Python development environment with eglot LSP and optional
lsp-mode / dape support.  Mirrors the pattern of feature-deno for dape
integration.  lsp-mode hooks are added only when feature-emacs-lsp is
present in the configuration."

  (define python-exe  (file-append python "/bin/python3"))
  (define pylsp-exe   (file-append python-lsp-server "/bin/pylsp"))

  (define (emacs-config config)
    (rde-elisp-configuration-service
     'emacs-python
     config
     (python-emacs-forms config python-exe pylsp-exe #:dape-port dape-port)
     #:authors
     '("Grigory Shepelev <shegeley@gmail.com>")))

  (define (get-home-services config)
    (list
     (when (get-value 'emacs config) (emacs-config config))
     (simple-service
      'python-add-packages
      home-profile-service-type
      (list python python-lsp-server python-debugpy tree-sitter-python))))

  (feature
   (name 'python)
   (values `((python . #t)
             (emacs-python . #t)))
   (home-services-getter get-home-services)))
