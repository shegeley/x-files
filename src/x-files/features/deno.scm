(define-module (x-files features deno)
 #:use-module (guix gexp)
 #:use-module (guix packages)
 #:use-module (guix download)

 #:use-module (x-files packages deno)
 #:use-module (x-files packages emacs deno)

 #:use-module ((contrib packages node-xyz) #:select (node-vscode-js-debug-1.86.0))

 #:use-module (rde features)
 #:use-module (rde features emacs)

 #:use-module (rde packages emacs-xyz)

 #:use-module (gnu services)
 #:use-module (gnu home services)

 #:use-module ((gnu packages node) #:select (node-lts))
 #:use-module (gnu packages emacs-xyz)
 #:use-module (gnu packages tree-sitter)

 #:export (feature-deno
           node-vscode-js-debug-latest))

(define node-vscode-js-debug-latest
  (let* [(version "1.97.1")
         (uri (string-append
               "https://github.com/microsoft/vscode-js-debug/"
               "releases/download/v" version
               "/js-debug-dap-v" version ".tar.gz"))
         (hash "135dj20maszb1xwsqq4mh3ah3rzbv2j3y066z56p4ilwbn4lgv9x")]
    (package
      (inherit node-vscode-js-debug-1.86.0)
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri uri)
         (sha256 (base32 hash))))
      (inputs (list node-lts)))))

(define* (feature-deno
          #:key
          (deno deno)
          (emacs-deno-mode emacs-deno-mode)
          (node-vscode-js-debug node-vscode-js-debug-latest)
          (idle-time 0.3))

  "Stolen from RDE and refactored to use with deno and deno-ts-mode. A lot removed.
   Only dape + eglot left. Add deno settings"

 (define deno-exe  (file-append deno "/bin/deno"))
 (define debug-exe (file-append node-vscode-js-debug
                    "/bin/dapDebugServer"))

 (define dape-config
   `((with-eval-after-load 'dape
       (with-eval-after-load 'deno-mode
         (let ((debug-exe ,debug-exe)
               (deno-exe  ,deno-exe))
           (setq dape-configs
                 (append
                  `((deno
                     modes (deno-ts-mode deno-tsx-mode deno-js-mode deno-jsx-mode)
                     command ,debug-exe
                     port 8123
                     ;; pwa-node
                     ;; https://stackoverflow.com/questions/63442436/what-is-the-pwa-node-type-launch-configuration-on-vscode
                     :type "pwa-node"
                     :runtimeExecutable ,deno-exe
                     :name "(Java/Type)script with Deno"
                     :request "launch"
                     :cwd dape-cwd
                     ;; ["run" "--inspect-brk"] (vector) builds to ("run" "--inspect-brk") (list) on this guix+elisp setup
                     ;; it took me ~ 3h to figure out + debug 💀
                     :runtimeArgs (vector "run" "--inspect-brk" "--unstable" "--allow-all")
                     :program dape-buffer-default
                     :attachSimplePort 9229
                     :port 9229))
                  `((chrome-frontend
                     modes (deno-ts-mode deno-tsx-mode deno-js-mode deno-jsx-mode)
                     command ,debug-exe
                     port 8123
                     :type "chrome"
                     :name "pwa-chrome"
                     :sourceMaps t
                     :trace t
                     :outputCapture "internalConsole"
                     :url ,(lambda () (read-string "Url: " "http://localhost:3000"))
                     :webRoot ,(lambda () (read-string "Root: " (funcall dape-cwd-fn)))))
                  `((deno-attach
                     modes (deno-ts-mode deno-tsx-mode deno-js-mode deno-jsx-mode)
                     command ,debug-exe
                     port 8123
                     type "pwa-node"
                     :name "JS/TS Node Attach"
                     :request "attach"
                     :port 9229))
                  dape-configs)))))))

 (define (emacs-config config)
   (rde-elisp-configuration-service
    'emacs-javascript
    config
    `((require 'deno-mode)

      (with-eval-after-load 'deno-mode
        (setq
         deno-bin             ,deno-exe
         deno-project-markers '("deno.json" "package.json")))

      (with-eval-after-load 'deno-mode
        (with-eval-after-load 'eglot
          (add-hook 'deno-ts-mode-hook  'eglot-ensure)
          (add-hook 'deno-tsx-mode-hook 'eglot-ensure)
          (add-hook 'deno-js-mode-hook  'eglot-ensure)
          (add-hook 'deno-jsx-mode-hook 'eglot-ensure)))

      ;; Snappy, inline as-you-type lint/type diagnostics.  Only when the
      ;; 'emacs-lsp feature value is present; otherwise omitted.
      ,@(if (get-value 'emacs-lsp config #f)
          `((with-eval-after-load 'eglot
              ;; re-send buffer changes to `deno lsp' shortly after typing stops,
              ;; so diagnostics refresh while editing (not just on save)
              (setq eglot-send-changes-idle-time ,idle-time)

              (defun deno/live-diagnostics ()
                "Make deno lint/type diagnostics snappy and visible in this buffer."
                ;; show the message right at the end of the offending line (Emacs 30+)
                (when (boundp 'flymake-show-diagnostics-at-end-of-line)
                  (setq-local flymake-show-diagnostics-at-end-of-line 'short))
                ;; safeguard: keep deno lint on even if deno-mode ever drops the
                ;; server initializationOptions
                (setq-local eglot-workspace-configuration
                            '(:deno (:enable t
                                     :lint t
                                     :unstable t))))

              (dolist (hook '(deno-ts-mode-hook
                              deno-tsx-mode-hook
                              deno-js-mode-hook
                              deno-jsx-mode-hook))
                (add-hook hook (function deno/live-diagnostics)))))
          '())

      ,@(if (get-value 'emacs-dape config #f) dape-config '()))
    #:authors
    '("Grigory Shepelev <shegeley@gmail.com>"
      "Demis Balbach <db@minikn.xyz>"
      "Andrew Tropin <andrew@trop.in>")
    #:elisp-packages
    (list
     emacs-deno-mode emacs-npm-mode emacs-flycheck-deno
     emacs-web-mode emacs-markdown-mode)))

 (define (get-home-services config)
  (list
   (when (get-value 'emacs config) (emacs-config config))
   (simple-service
    'type&java-script-add-packages
    home-profile-service-type
    (list deno tree-sitter-typescript tree-sitter-javascript))))

 (feature
  (name 'deno)
  (values `((deno . #t)
            (javascript . #t)
            (typescript . #t)
            (emacs-javascript . #t)
            (emacs-typescript . #t)))
  (home-services-getter get-home-services)))
