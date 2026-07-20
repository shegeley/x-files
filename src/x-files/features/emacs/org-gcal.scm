(define-module (x-files features emacs org-gcal)
  #:use-module ((rde features)        #:select (feature))
  #:use-module ((rde features emacs)  #:select (rde-elisp-configuration-service))
  #:use-module ((x-files packages emacs org-gcal) #:select (emacs-org-gcal))

  #:export (feature-emacs-org-gcal))

;; org-gcal — two-way Org <-> Google Calendar sync (see (x-files packages emacs
;; org-gcal)).  The Google OAuth app credentials are NOT baked into the store:
;; they're fetched at load time from `pass' via CLIENT-ID-CMD / CLIENT-SECRET-CMD
;; (org-gcal loads lazily, so pass only runs on the first `org-gcal-sync').
;; FETCH-FILE-ALIST maps calendar ids to Org files, e.g.
;;   '(("me@gmail.com" . "~/org/calendar.org")).

(define* (feature-emacs-org-gcal
          #:key
          (client-id-cmd     "pass show google/org-gcal/client-id")
          (client-secret-cmd "pass show google/org-gcal/client-secret")
          (fetch-file-alist  '()))
  "Configure org-gcal with OAuth app creds read from `pass' (CLIENT-ID-CMD,
CLIENT-SECRET-CMD) and FETCH-FILE-ALIST mapping calendar ids to Org files."

  (define f-name 'emacs-org-gcal)

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      f-name config
      `((with-eval-after-load 'org-gcal
          (setq org-gcal-client-id
                (string-trim-right (shell-command-to-string ,client-id-cmd))
                org-gcal-client-secret
                (string-trim-right (shell-command-to-string ,client-secret-cmd))
                org-gcal-fetch-file-alist ',fetch-file-alist)))
      #:elisp-packages (list emacs-org-gcal))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
