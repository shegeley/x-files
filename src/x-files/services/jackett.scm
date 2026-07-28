(define-module (x-files services jackett)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system shadow) #:select (account-service-type
                                              user-account
                                              user-group))
  #:use-module ((x-files packages jackett) #:select (jackett))

  #:export (jackett-configuration
            jackett-service-type))

;; Configuration is a plain alist (no record types): a `jackett-configuration'
;; helper fills defaults, and the shepherd/activation/account builders read it
;; back with `assoc-ref'.

(define* (jackett-configuration
          #:key
          (package        jackett)
          (user           "jackett")
          (group          "jackett")
          (data-folder    "/var/lib/jackett")
          ;; #f keeps whatever the on-disk ServerConfig.json holds (Jackett's
          ;; own default is 9117); a number overrides it on every start.
          (port           9117)
          ;; #f => --ListenPrivate (localhost only, the safe default behind a
          ;; reverse proxy); #t => --ListenPublic (bind all interfaces).
          (listen-public? #f)
          ;; The store is read-only, so the bundled self-updater can never
          ;; write; disable it (and never auto-restart from an "update").
          (no-updates?    #t)
          ;; Extra raw CLI arguments appended verbatim, e.g. '("--Tracing").
          (extra-options  '()))
  `((package        . ,package)
    (user           . ,user)
    (group          . ,group)
    (data-folder    . ,data-folder)
    (port           . ,port)
    (listen-public? . ,listen-public?)
    (no-updates?    . ,no-updates?)
    (extra-options  . ,extra-options)))

(define (jackett-arguments config)
  "Build the jackett command-line argument list (of strings) from CONFIG."
  (let ((data-folder (assoc-ref config 'data-folder))
        (port        (assoc-ref config 'port))
        (public?     (assoc-ref config 'listen-public?))
        (no-updates? (assoc-ref config 'no-updates?))
        (extra       (assoc-ref config 'extra-options)))
    (append
     (list "--NoRestart"
           (string-append "--DataFolder=" data-folder))
     (if no-updates? '("--NoUpdates") '())
     (if port (list (string-append "--Port=" (number->string port))) '())
     (list (if public? "--ListenPublic" "--ListenPrivate"))
     extra)))

(define (jackett-shepherd-service config)
  (let ((pkg         (assoc-ref config 'package))
        (user        (assoc-ref config 'user))
        (group       (assoc-ref config 'group))
        (data-folder (assoc-ref config 'data-folder))
        (args        (jackett-arguments config)))
    (list
     (shepherd-service
      (provision '(jackett))
      (requirement '(user-processes file-systems networking))
      (documentation "Run the Jackett tracker-proxy web server")
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/jackett") #$@args)
                #:user  #$user
                #:group #$group
                #:log-file "/var/log/jackett.log"
                #:environment-variables
                ;; .NET wants a writable HOME (first-run/.dotnet, temp state).
                (list (string-append "HOME=" #$data-folder)
                      (string-append "XDG_CONFIG_HOME=" #$data-folder)
                      "DOTNET_CLI_TELEMETRY_OPTOUT=1"
                      "DOTNET_NOLOGO=1")))
      (stop  #~(make-kill-destructor))
      (respawn? #t)))))

(define (jackett-activation config)
  (let ((data-folder (assoc-ref config 'data-folder))
        (user        (assoc-ref config 'user)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((pw  (getpwnam #$user))
                 (uid (passwd:uid pw))
                 (gid (passwd:gid pw)))
            (mkdir-p #$data-folder)
            (chown #$data-folder uid gid))))))

(define (jackett-accounts config)
  (let ((user        (assoc-ref config 'user))
        (group       (assoc-ref config 'group))
        (data-folder (assoc-ref config 'data-folder)))
    (list
     (user-group  (name group) (system? #t))
     (user-account
      (name    user)
      (group   group)
      (system? #t)
      (home-directory data-folder)
      (comment "Jackett tracker-proxy daemon")))))

(define-public jackett-service-type
  (service-type
   (name 'jackett)
   (description "Run Jackett, a proxy server translating tracker queries into
Torznab/TorrentPotato results for PVR apps (Sonarr, Radarr, Lidarr, ...).")
   (default-value (jackett-configuration))
   (extensions
    (list
     (service-extension shepherd-root-service-type jackett-shepherd-service)
     (service-extension activation-service-type    jackett-activation)
     (service-extension account-service-type       jackett-accounts)))))
