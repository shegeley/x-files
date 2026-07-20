(define-module (x-files packages emacs org-gcal)
  #:use-module ((guix licenses)            #:prefix license:)
  #:use-module ((guix packages)            #:select (package origin base32))
  #:use-module ((guix git-download)        #:select (git-fetch git-reference git-file-name git-version))
  #:use-module ((guix build-system emacs)  #:select (emacs-build-system))
  #:use-module ((gnu packages emacs-build) #:select (emacs-dash))
  #:use-module ((gnu packages emacs-xyz)   #:select (emacs-aio
                                                     emacs-alert
                                                     emacs-persist
                                                     emacs-request))

  #:export (emacs-oauth2-auto emacs-org-gcal))

;; org-gcal (Org <-> Google Calendar sync) and its OAuth dependency oauth2-auto,
;; neither of which is in Guix.  oauth2-auto mints/refreshes Google OAuth 2.0
;; tokens; org-gcal drives the Calendar API from it.  Integration (client-id /
;; client-secret from `pass', org-gcal-fetch-file-alist over ~/org) lives in the
;; system config, not here.

(define-public emacs-oauth2-auto
  (let [(commit "b16ac19a91441bc899322578642959b19bd951e3")
        (version "0.1")]
    (package
      (name "emacs-oauth2-auto")
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rhaps0dy/emacs-oauth2-auto")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "0b825d4zkl7ni8q7xc44zqm6lkhmh925l0ajvgyak5wk6bxridli"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs (list emacs-aio emacs-alert emacs-dash))
      (home-page "https://github.com/rhaps0dy/emacs-oauth2-auto")
      (synopsis "Automatically refreshing OAuth 2.0 tokens for Emacs")
      (description "Library that transparently obtains and refreshes OAuth 2.0
tokens (e.g. for Google APIs), persisting them so callers get a valid token on
demand.")
      (license license:gpl3+))))

(define-public emacs-org-gcal
  (let [(commit "7304b592c283944db54ac83201d7be6f13a1f447")
        (version "0.4.3")]
    (package
      (name "emacs-org-gcal")
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacsmirror/org-gcal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "1b701j2m4cn17bh8n3xmxh5d4bwg5i4spc1v9i9rcq96kpj4mkbj"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (propagated-inputs
       (list emacs-aio emacs-alert emacs-oauth2-auto
             emacs-persist emacs-request))
      (home-page "https://github.com/kidd/org-gcal.el")
      (synopsis "Org-mode <-> Google Calendar synchronization")
      (description "Two-way sync between Org-mode files and Google Calendar:
pull calendar events into Org and push Org entries back, driven by OAuth 2.0
credentials (via oauth2-auto).")
      (license license:gpl3+))))
