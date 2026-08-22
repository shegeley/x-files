(define-module (x-files services yaak)
  #:use-module (guix gexp)
  #:use-module ((gnu services)           #:select (service-extension
                                                    service-type))
  #:use-module ((gnu services shepherd)  #:select (shepherd-service))
  #:use-module ((gnu home services)      #:select (home-profile-service-type
                                                    home-activation-service-type))
  #:use-module ((gnu home services shepherd) #:select (home-shepherd-service-type))
  #:use-module ((gnu packages python)    #:select (python-wrapper))
  #:use-module ((rde lib file)           #:select (find-file-in-load-path))
  #:use-module ((x-files packages yaak)  #:select (yaak))
  #:export (yaak-home-service-type
            yaak-default-config))

;; Runs Yaak (see (x-files packages yaak)) as an always-on home Shepherd
;; service -- a real GTK window auto-starts at login, and Yaak's own
;; built-in MCP server (if enabled) stays reachable the whole session, the
;; same always-on shape the old OCI-container-based setup had (just native
;; now, no podman/linuxserver.io image).
;;
;; Optionally installs and registers Yaak's official MCP-server plugin
;; (https://github.com/mountain-loop/yaak/tree/main/plugins-external/mcp-server)
;; if its build output is supplied via MCP-PLUGIN-INDEX-JS/
;; MCP-PLUGIN-PACKAGE-JSON -- upstream's in-app plugin download for it 500s,
;; so there is no way to fetch it live; pass in your own prebuilt copy (file-
;; like objects) if you have one, or leave both #f to skip.

;; Replicated from (shepherd support): importing that module at the channel
;; top level breaks `guix pull' -- shepherd is not on the channel build load
;; path. Value matches (shepherd support) %user-log-dir exactly. Same trick
;; as (x-files services browser-history-manager).
(define %user-log-dir
  (string-append (or (getenv "XDG_STATE_HOME")
                     (string-append (or (getenv "HOME") "") "/.local/state"))
                 "/shepherd"))

(define yaak-mcp-setup-script
  (local-file
   (find-file-in-load-path
    "x-files/packages/aux/yaak-mcp-setup/yaak-mcp-setup.py")))

(define yaak-default-config
  `((package                 . ,yaak)
    (log-file                . ,(string-append %user-log-dir "/yaak.log"))
    (mcp-plugin-index-js     . #f)
    (mcp-plugin-package-json . #f)))

(define (cfg c k) (assoc-ref c k))

(define (yaak-shepherd-service config)
  (list
   (shepherd-service
    (provision '(yaak))
    (start #~(make-forkexec-constructor
              (list #$(file-append (cfg config 'package) "/bin/yaak-app-client"))
              #:log-file #$(cfg config 'log-file)))
    (stop #~(make-kill-destructor))
    (documentation "Yaak API client -- auto-started desktop window + MCP server."))))

(define (yaak-mcp-plugin-activation config)
  "Install + register Yaak's official MCP-server plugin, if its build
output was supplied in CONFIG.  A no-op gexp otherwise."
  (let ((index-js (cfg config 'mcp-plugin-index-js))
        (pkg-json (cfg config 'mcp-plugin-package-json)))
    (if (and index-js pkg-json)
        #~(begin
            (use-modules (guix build utils))
            (let* ((home       (getenv "HOME"))
                   (data-dir   (string-append home "/.local/share/app.yaak.desktop"))
                   (plugin-dir (string-append data-dir "/plugins/@yaak/mcp-server"))
                   (db-path    (string-append data-dir "/db.sqlite")))
              (mkdir-p (string-append plugin-dir "/build"))
              (copy-file #$index-js (string-append plugin-dir "/build/index.js"))
              (copy-file #$pkg-json (string-append plugin-dir "/package.json"))
              ;; Only register once Yaak has run at least once (db.sqlite
              ;; exists); harmless no-op activation runs before that.
              (when (file-exists? db-path)
                (system* #$(file-append python-wrapper "/bin/python3")
                         #$yaak-mcp-setup-script
                         plugin-dir db-path))))
        #~(begin))))

(define-public yaak-home-service-type
  (service-type
   (name 'yaak)
   (default-value yaak-default-config)
   (extensions
    (list
     (service-extension home-profile-service-type
                        (lambda (config) (list (cfg config 'package))))
     (service-extension home-shepherd-service-type
                        yaak-shepherd-service)
     (service-extension home-activation-service-type
                        yaak-mcp-plugin-activation)))
   (description
    "Install Yaak (see @code{(x-files packages yaak)}) and auto-start it as
a home Shepherd service, optionally installing and registering its
official MCP-server plugin.")))
