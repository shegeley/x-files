(define-module (x-files services grafana)
  #:use-module (guix gexp)
  #:use-module ((gnu services)           #:select (service-extension
                                                   service-type))
  #:use-module ((gnu services shepherd)  #:select (shepherd-service
                                                   shepherd-root-service-type))
  #:use-module ((gnu services)           #:select (activation-service-type
                                                   profile-service-type))
  #:use-module ((gnu system shadow)      #:select (account-service-type
                                                   user-account
                                                   user-group))
  #:use-module ((x-files packages grafana) #:select (grafana-bin))

  #:export (grafana-service-type
            grafana-default-config))

;; Config is a plain alist.  Keys:
;;   package    — grafana-bin package
;;   port       — HTTP listen port (string, default "3000")
;;   addr       — HTTP bind address (string, default "")
;;   data-dir   — runtime data directory (default "/var/lib/grafana")
;;   log-dir    — log directory (default "/var/log/grafana")
;;   cfg-path   — path to a custom grafana.ini (default #f = use built-in defaults)

(define grafana-default-config
  `((package  . ,grafana-bin)
    (port     . "3000")
    (addr     . "")
    (data-dir . "/var/lib/grafana")
    (log-dir  . "/var/log/grafana")
    (cfg-path . #f)))

(define (cfg config key)
  (assoc-ref config key))

(define (grafana-homepath config)
  (file-append (cfg config 'package) "/share/grafana"))

(define (grafana-accounts config)
  (list
   (user-account
    (name "grafana")
    (group "grafana")
    (system? #t)
    (home-directory (cfg config 'data-dir))
    (comment "Grafana server user"))
   (user-group
    (name "grafana"))))

(define (grafana-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$(cfg config 'data-dir))
      (mkdir-p #$(cfg config 'log-dir))
      (let* ((pw  (getpwnam "grafana"))
             (uid (passwd:uid pw))
             (gid (passwd:gid pw)))
        (for-each (lambda (dir) (chown dir uid gid))
                  (list #$(cfg config 'data-dir)
                        #$(cfg config 'log-dir))))))

(define (grafana-shepherd-service config)
  (list
   (shepherd-service
    (provision '(grafana))
    (requirement '(file-systems networking))
    (documentation "Run the Grafana observability server.")
    (start
     #~(make-forkexec-constructor
        (append
         (list #$(file-append (cfg config 'package) "/bin/grafana")
               "server"
               "--homepath" #$(grafana-homepath config))
         (let ((cfg-path #$(cfg config 'cfg-path)))
           (if cfg-path (list "--config" cfg-path) '())))
        #:user  "grafana"
        #:group "grafana"
        #:log-file (string-append #$(cfg config 'log-dir) "/grafana.log")
        #:environment-variables
        (list
         (string-append "GF_SERVER_HTTP_PORT=" #$(cfg config 'port))
         (string-append "GF_SERVER_HTTP_ADDR=" #$(cfg config 'addr))
         (string-append "GF_PATHS_DATA="  #$(cfg config 'data-dir))
         (string-append "GF_PATHS_LOGS="  #$(cfg config 'log-dir))
         (string-append "GF_PATHS_PLUGINS=" #$(cfg config 'data-dir) "/plugins"))))
    (stop #~(make-kill-destructor))
    (auto-start? #t))))

(define-public grafana-service-type
  (service-type
   (name 'grafana)
   (description "Grafana observability and dashboarding server.")
   (extensions
    (list
     (service-extension account-service-type    grafana-accounts)
     (service-extension activation-service-type grafana-activation)
     (service-extension profile-service-type
                        (lambda (config) (list (cfg config 'package))))
     (service-extension shepherd-root-service-type grafana-shepherd-service)))
   (default-value grafana-default-config)))
