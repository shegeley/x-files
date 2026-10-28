(define-module (x-files services gitlab-runner)
  #:use-module (guix gexp)
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module ((guix packages) #:select (package-transitive-propagated-inputs))
  #:use-module ((gnu services) #:select (service-type
                                         service-extension
                                         activation-service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service
                                                  shepherd-root-service-type))
  #:use-module ((gnu system shadow) #:select (account-service-type
                                              user-account
                                              user-group))
  #:use-module ((gnu packages bash) #:select (bash))
  #:use-module ((gnu packages base) #:select (coreutils
                                              findutils
                                              grep
                                              sed
                                              tar))
  #:use-module ((gnu packages compression) #:select (gzip
                                                     xz))
  #:use-module ((gnu packages version-control) #:select (git-minimal))
  #:use-module ((gnu packages ssh) #:select (openssh))
  #:use-module ((gnu packages package-management) #:select (guix))
  #:use-module ((gnu packages nss) #:select (nss-certs))
  #:use-module ((gnu packages guile-xyz) #:select (guile-toml))
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((srfi srfi-1) #:select (every
                                        delete-duplicates))
  #:use-module ((x-files packages gitlab-runner) #:select (gitlab-runner-bin))
  #:use-module ((x-files utils alist) #:select (match-alist))
  #:export (gitlab-runner
            gitlab-runner-configuration
            gitlab-runner-service-type))

(define (runtime-path? value)
  (and (string? value)
       (string-prefix? "/" value)
       (not (string-prefix? "/gnu/store/" value))
       (not (member value '("/"
                            "/var"
                            "/var/lib"
                            "/etc"
                            "/tmp"
                            "/gnu/store")))))

(define* (gitlab-runner #:key
                        name
                        token-file
                        (url "https://gitlab.com")
                        (settings '(("executor" . "shell")
                                    ("shell"    . "bash"))))
  "Declare @var{name} at @var{url} with public TOML @var{settings} and a
runtime authentication file, @var{token-file}.  Create the project runner in
GitLab first; tags and protection are set there."
  (unless (and (string? name)
               (not (string-null? name))
               (string? url)
               (runtime-path? token-file)
               (every (lambda (key) (not (assoc key settings)))
                      '("name"
                        "url"
                        "token"
                        "id"
                        "token_obtained_at"
                        "token_expires_at")))
    (error "Runner needs a name, URL, runtime token-file, and public settings"))
  `((url . ,url)
    (name . ,name)
    (settings . ,settings)
    (token-file . ,token-file)))

(define* (gitlab-runner-configuration #:key
                                      (package gitlab-runner-bin)
                                      (runners '())
                                      (settings '(("concurrent"     . 1)
                                                  ("check_interval" . 3)))
                                      (state-directory "/var/lib/gitlab-runner")
                                      (log-files "/var/log/gitlab-runner.log")
                                      (user "gitlab-runner")
                                      (group "gitlab-runner")
                                      (extra-packages '())
                                      (stop-timeout 3600))
  "Return an alist configuring a dedicated unprivileged Runner manager.
@var{runners} declares project runners; @var{settings} supplies global TOML
settings.  State in @var{state-directory} survives rollback.  Set
@var{stop-timeout}, in seconds, above the project job timeout for graceful
draining.  @var{log-files} is the absolute path of the manager log,
defaulting to @file{/var/log/gitlab-runner.log}; its parent directory must
already exist."
  (match-alist settings (("concurrent" concurrent))
    (let ((unique-runner-names?
           (= (length runners)
              (length
               (delete-duplicates
                (map (lambda (runner)
                       (match-alist runner (name) name))
                     runners))))))
      (unless (and (runtime-path? state-directory)
                   (runtime-path? log-files)
                   (integer? stop-timeout)
                   (> stop-timeout 0)
                   (integer? concurrent)
                   (> concurrent 0)
                   (not (assoc "runners" settings))
                   unique-runner-names?)
        (error "Invalid Runner state directory, log path, timeout, settings, or duplicate names"))
      `((package . ,package)
        (runners . ,runners)
        (settings . ,settings)
        (state-directory . ,state-directory)
        (log-files . ,log-files)
        (user . ,user)
        (group . ,group)
        (extra-packages . ,extra-packages)
        (stop-timeout . ,stop-timeout)))))

(define (runner-program config)
  (match-alist config (package state-directory settings runners)
    (let ((binary (file-append package "/bin/gitlab-runner")))
      (with-extensions (cons* guix guile-toml guile-gcrypt
                              (map cadr (package-transitive-propagated-inputs guix)))
        (with-imported-modules
            (source-module-closure '((x-files build gitlab-runner))
                                   #:select? (lambda (name)
                                               (eq? 'x-files (car name))))
          (program-file "start-gitlab-runner"
            #~(begin
                (use-modules ((x-files build gitlab-runner) #:select (configure-runner!)))
                (umask #o077)
                (catch #t
                  (lambda ()
                    (configure-runner!
                     '#$settings
                     '#$runners
                     #$state-directory
                     (lambda (candidate)
                       ;; Lint output may contain credentials. Keep it out of logs.
                       (let ((null (open-file "/dev/null" "w")))
                         (with-output-to-port null
                           (lambda ()
                             (with-error-to-port null
                               (lambda ()
                                 (zero? (system* #$binary "lint" "--config" candidate)))))))))
                    (execl #$binary
                           "gitlab-runner"
                           "run"
                           "--config"
                           (string-append #$state-directory "/config.toml")
                           "--working-directory"
                           #$state-directory))
                  (lambda _
                    (display "GitLab Runner configuration/start failed; check token-file permissions and public settings.\n"
                             (current-error-port))
                    (exit 1))))))))))

(define (runner-shepherd-services config)
  (match-alist config (state-directory user group log-files extra-packages stop-timeout)
    (let ((certificate-environment
           #~(let* ((directory (string-append #$nss-certs "/etc/ssl/certs"))
                    (bundle (string-append directory "/ca-certificates.crt")))
               (list (string-append "SSL_CERT_DIR=" directory)
                     (string-append "SSL_CERT_FILE=" bundle)
                     (string-append "GIT_SSL_CAINFO=" bundle))))
          (packages (append (list bash
                                  coreutils
                                  findutils
                                  grep
                                  sed
                                  tar
                                  gzip
                                  xz
                                  git-minimal
                                  openssh
                                  guix)
                            extra-packages)))
      (list
       (shepherd-service
         (provision '(gitlab-runner))
         (requirement '(user-processes file-systems networking))
         (documentation "Run GitLab CI jobs as a dedicated user, draining jobs on stop.")
         (start #~(make-forkexec-constructor
                   (list #$(runner-program config))
                   #:user #$user
                   #:group #$group
                   #:directory #$state-directory
                   #:file-creation-mask #o077
                   #:log-file #$log-files
                   #:environment-variables
                   (append
                    (list (string-append "HOME=" #$state-directory)
                          "LANG=C.UTF-8")
                    #$certificate-environment
                    (list (string-append
                           "PATH="
                           (string-join
                            '#$(map (lambda (p) (file-append p "/bin")) packages)
                            ":"))))))
         (stop #~(lambda (process)
                   ;; make-kill-destructor signals the group, which aborts jobs.
                   (terminate-process (if (integer? process)
                                          process
                                          (process-id process))
                                      SIGQUIT
                                      #:grace-period #$stop-timeout)
                   #f)))))))

(define (runner-activation config)
  (match-alist config (user state-directory)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules ((guix build utils) #:select (mkdir-p)))
          (catch #t
            (lambda ()
              (let ((account (getpwnam #$user))
                    (directory #$state-directory))
                (mkdir-p directory)
                (unless (and (eq? 'directory (stat:type (lstat directory)))
                             (string=? directory (canonicalize-path directory)))
                  (error "Runner state must be a directory without symlinks"))
                (chmod directory #o700)
                (chown directory (passwd:uid account) (passwd:gid account))))
            (lambda _
              (display "GitLab Runner state activation failed; existing state retained.\n" (current-error-port))
              #t))))))

(define (runner-accounts config)
  (match-alist config (user group state-directory)
    (list (user-group (name group)
                      (system? #t))
          (user-account
            (name user)
            (group group)
            (system? #t)
            (home-directory state-directory)
            (comment "GitLab CI runner")))))

(define gitlab-runner-service-type
  (service-type
    (name 'gitlab-runner)
    (description "Declarative GitLab Runner with runtime credentials and persistent state.")
    (extensions
     (list (service-extension account-service-type runner-accounts)
           (service-extension activation-service-type runner-activation)
           (service-extension shepherd-root-service-type runner-shepherd-services)))
    (default-value (gitlab-runner-configuration))))
