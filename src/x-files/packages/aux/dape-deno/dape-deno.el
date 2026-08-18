;;; dape-deno.el --- dape configs for Deno debugging -*- lexical-binding: t -*-

;;; Commentary:

;; Registers `deno', `chrome-frontend', and `deno-attach' dape debug
;; configurations for deno-[ts|tsx|js|jsx]-mode buffers, using
;; vscode-js-debug's dapDebugServer as the DAP adapter, and either
;; launching Deno with `--inspect-brk' or attaching to an
;; already-running one.  `dape-deno-debug-exe' and `dape-deno-deno-exe'
;; are patched to absolute Guix store paths at build time.

;;; Code:

(defcustom dape-deno-debug-exe "dapDebugServer"
  "Absolute path to vscode-js-debug's dapDebugServer.
Patched to the real Guix store path at build time."
  :type 'string)

(defcustom dape-deno-deno-exe "deno"
  "Absolute path to the deno binary.
Patched to the real Guix store path at build time."
  :type 'string)

(defvar dape-deno--launch-config
  `(deno
    modes (deno-ts-mode deno-tsx-mode deno-js-mode deno-jsx-mode)
    command ,dape-deno-debug-exe
    port 8123
    ;; pwa-node
    ;; https://stackoverflow.com/questions/63442436/what-is-the-pwa-node-type-launch-configuration-on-vscode
    :type "pwa-node"
    :runtimeExecutable ,dape-deno-deno-exe
    :name "(Java/Type)script with Deno"
    :request "launch"
    :cwd dape-cwd
    ;; ["run" "--inspect-brk"] (vector) builds to ("run" "--inspect-brk") (list) on this guix+elisp setup
    :runtimeArgs (vector "run" "--inspect-brk" "--unstable" "--allow-all")
    :program dape-buffer-default
    :attachSimplePort 9229
    :port 9229)
  "dape config: launch a Deno script directly, stopped at its first line.")

(defvar dape-deno--chrome-frontend-config
  `(chrome-frontend
    modes (deno-ts-mode deno-tsx-mode deno-js-mode deno-jsx-mode)
    command ,dape-deno-debug-exe
    port 8123
    :type "chrome"
    :name "pwa-chrome"
    :sourceMaps t
    :trace t
    :outputCapture "internalConsole"
    :url ,(lambda () (read-string "Url: " "http://localhost:3000"))
    :webRoot ,(lambda () (read-string "Root: " (funcall dape-cwd-fn))))
  "dape config: attach a Chrome frontend debug session (prompts for URL/root).")

(defvar dape-deno--attach-config
  `(deno-attach
    modes (deno-ts-mode deno-tsx-mode deno-js-mode deno-jsx-mode)
    command ,dape-deno-debug-exe
    port 8123
    type "pwa-node"
    :name "JS/TS Node Attach"
    :request "attach"
    :port 9229)
  "dape config: attach to an already-running `deno run --inspect' process.")

(with-eval-after-load 'dape
  (with-eval-after-load 'deno-mode
    (dolist (config (list dape-deno--launch-config
                          dape-deno--chrome-frontend-config
                          dape-deno--attach-config))
      (add-to-list 'dape-configs config))))

(provide 'dape-deno)
;;; dape-deno.el ends here
