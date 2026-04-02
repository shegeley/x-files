(define-module (x-files features browser-mcp)
  #:use-module (guix gexp)

  #:use-module (rde features)
  #:use-module ((gnu services) #:select (simple-service))
  #:use-module ((gnu home services) #:select (home-profile-service-type
                                               home-activation-service-type))
  #:use-module ((gnu home services shepherd) #:select (home-shepherd-service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service))

  #:use-module ((gnu packages guile) #:select (guile-json-4))

  #:use-module ((x-files packages geckodriver) #:select (geckodriver))
  #:use-module ((x-files packages mcp browser) #:select (rust-mcp-browser))

  #:export (feature-browser-mcp))

;; Browser automation MCP server for Claude Code.
;;
;; On `guix home reconfigure`:
;;   1. Installs geckodriver + rust-browser-mcp into the profile.
;;   2. Starts geckodriver as a Shepherd home service.
;;   3. Registers the MCP server in ~/.claude.json (user scope) so it is
;;      available in every Claude Code session automatically.
;;
;; NOTE: rust-browser-mcp README and error messages advertise WEBDRIVER_URL,
;; but the binary actually reads WEBDRIVER_ENDPOINT.  WEBDRIVER_URL is silently
;; ignored and falls through to a chromedriver lookup, breaking Firefox/geckodriver
;; setups.  Confirmed by testing v0.3.1 — use WEBDRIVER_ENDPOINT.
;;
;; guile-json note: json->scm returns JSON objects as alists in Guix's
;; version of guile-json, so alist operations (assoc-ref, etc.) are used
;; throughout.

(define (claude-json-register-mcp webdriver-url)
  ;; Idempotently register the browser MCP server in ~/.claude.json (user scope).
  ;; guile-json represents JSON objects as alists; alist-cons/alist-delete are
  ;; used to update only the relevant keys without touching any other state.
  (with-extensions (list guile-json-4)
    #~(begin
        (use-modules (json) (srfi srfi-1))

        (define path
          (string-append (getenv "HOME") "/.claude.json"))

        (define desired-entry
          `(("type"    . "stdio")
            ("command" . "rust-browser-mcp")
            ("args"    . #())
            ("env"     . (("WEBDRIVER_ENDPOINT" . #$webdriver-url)))))

        (define data
          (if (file-exists? path)
              (call-with-input-file path json->scm)
              '()))

        (define mcp-servers
          (or (assoc-ref data "mcpServers") '()))

        (define current-entry
          (assoc-ref mcp-servers "browser"))

        (unless (equal? current-entry desired-entry)
          (let* ((new-mcp  (alist-cons "browser" desired-entry
                                       (alist-delete "browser" mcp-servers)))
                 (new-data (alist-cons "mcpServers" new-mcp
                                       (alist-delete "mcpServers" data))))
            (call-with-output-file path
              (lambda (port) (scm->json new-data port)))
            (display "browser-mcp: registered in ~/.claude.json\n"))))))

(define* (feature-browser-mcp
          #:key
          (auto-start? #t)
          (port 4444))

  (define f-name        'browser-mcp)
  (define port-str      (number->string port))
  (define webdriver-url (string-append "http://localhost:" port-str))

  (define (get-home-services config)
    (list
     (simple-service
      'browser-mcp-packages
      home-profile-service-type
      (list geckodriver rust-mcp-browser))

     (simple-service
      'geckodriver-shepherd-service
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(geckodriver))
        (documentation "Firefox WebDriver bridge for browser MCP automation.")
        (auto-start? auto-start?)
        (stop  #~(make-kill-destructor))
        (start #~(make-forkexec-constructor
                  (list #$(file-append geckodriver "/bin/geckodriver")
                        "--port" #$port-str))))))

     (simple-service
      'browser-mcp-claude-registration
      home-activation-service-type
      (claude-json-register-mcp webdriver-url))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (geckodriver-port . ,port)
             (webdriver-url . ,webdriver-url)))
   (home-services-getter get-home-services)))
