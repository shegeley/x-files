(define-module (x-files features emacs gitlab-ci)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module ((srfi srfi-13) #:select (string-prefix?))

  #:use-module ((x-files packages emacs gitlab-ci) #:select (emacs-gitlab-ci))

  #:export (feature-emacs-gitlab-ci))

(define* (feature-emacs-gitlab-ci
          #:key
          (emacs-gitlab-ci emacs-gitlab-ci)
          (hosts '("gitlab.com"))
          (token-directory #f))
  "Watch GitLab CI/CD pipelines and jobs from Emacs via lab.el
(@code{lab-watch-pipeline-for-last-commit}, @code{lab-show-job-logs}, etc.).
HOSTS is a list of bare hostnames (e.g. '(\"gitlab.atlasdv.ru\" \"gitlab.com\")).
Each host's token is read independently, at runtime, from
TOKEN-DIRECTORY/HOST.token (default: emacs-gitlab-ci's own
gitlab-ci-token-directory) -- never baked into the store. A host with no
readable token file falls back to auth-source."

  (define f-name 'emacs-gitlab-ci)
  (define (host-url h)
    (if (string-prefix? "http" h) h (string-append "https://" h)))
  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      f-name config
      `((setq gitlab-ci-hosts ',(map host-url hosts))
        ,@(if token-directory
              `((setq gitlab-ci-token-directory ,token-directory))
              '())
        (require 'gitlab-ci))
      #:elisp-packages (list emacs-gitlab-ci))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
