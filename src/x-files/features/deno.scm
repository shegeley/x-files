(define-module (x-files features deno)
 #:use-module ((guix gexp) #:select (file-append))

 #:use-module (x-files packages deno)
 #:use-module (x-files packages emacs deno)
 #:use-module ((x-files packages emacs dape-deno) #:select (emacs-dape-deno))

 #:use-module (rde features)
 #:use-module (rde features emacs)

 #:use-module (rde packages emacs-xyz)

 #:use-module (gnu services)
 #:use-module (gnu home services)

 #:use-module (gnu packages emacs-xyz)
 #:use-module (gnu packages tree-sitter)

 #:export (feature-deno))

;; dape debug configs (the `deno'/`chrome-frontend'/`deno-attach' entries)
;; live in their own package, (x-files packages emacs dape-deno) -- built
;; from packages/aux/dape-deno/dape-deno.el, with the dapDebugServer/deno
;; store paths baked in at build time via `emacs-substitute-variables'
;; (patch-exe-paths phase) rather than spliced in as gexp'd string literals
;; here.  This service just requires that file; kept as a top-level define,
;; outside `feature-deno's body, since it has nothing else feature-local to
;; close over.
(define (dape-deno-service config)
  (rde-elisp-configuration-service
   'dape-deno config
   '((require 'dape-deno))
   #:elisp-packages (list emacs-dape-deno)))

(define* (feature-deno
          #:key
          (deno deno)
          (emacs-deno-mode emacs-deno-mode)
          (idle-time 0.3))

  "Stolen from RDE and refactored to use with deno and deno-ts-mode. A lot removed.
   Only dape + eglot left. Add deno settings"

 (define deno-exe  (file-append deno "/bin/deno"))

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
          '()))
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
   (when (get-value 'emacs-dape config #f) (dape-deno-service config))
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
