(define-module (x-files services stalwart)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu packages admin)  #:select (shadow))
  #:use-module ((gnu packages guile)  #:select (guile-json-4))
  #:use-module ((gnu system shadow)   #:select (account-service-type
                                                user-account
                                                user-group))
  #:use-module ((x-files packages stalwart) #:select (stalwart))
  #:export (stalwart-rocksdb-datastore
            stalwart-configuration
            stalwart-service-type))

;;;
;;; Stalwart mail server (v0.16) service.
;;;
;;; Stalwart 0.16 reads ONLY its data-store location from the `--config' file,
;;; which is JSON (not TOML): a single DataStore object such as
;;;
;;;   {"@type":"RocksDb","path":"/var/lib/stalwart"}
;;;
;;; Every other setting (listeners, directory, authentication, ...) lives in the
;;; database and is configured at runtime through the web admin / JMAP API.
;;;
;;; When no config.json exists yet, Stalwart starts in *bootstrap mode*: it binds
;;; HTTP on port 8080, prints a one-time temporary administrator password to its
;;; log, and serves a setup wizard at /admin that writes config.json, provisions
;;; the database and restarts into normal operation.  This service therefore
;;; defaults to bootstrap mode (no config.json pre-written).
;;;
;;; Configuration is modeled as a plain alist (no records).  The optional
;;; `datastore' field is itself an alist describing the DataStore object; when
;;; set, it is serialized to config.json with guile-json so the server skips the
;;; bootstrap wizard for the data store.
;;;

(define* (stalwart-rocksdb-datastore #:key (path "/var/lib/stalwart"))
  "DataStore alist for an embedded RocksDB store at PATH.  Serializes to
@code{@{\"@type\":\"RocksDb\",\"path\":\"...\"@}}."
  `(("@type" . "RocksDb")
    ("path"  . ,path)))

(define* (stalwart-configuration
          #:key
          (package        stalwart)
          (data-dir       "/var/lib/stalwart")
          (config-file    #f)        ; default <data-dir>/etc/config.json
          (recovery-admin #f)        ; "user:password" -> STALWART_RECOVERY_ADMIN
          (datastore      #f))       ; DataStore alist; #f => bootstrap mode
  `((package        . ,package)
    (data-dir       . ,data-dir)
    (config-file    . ,(or config-file (string-append data-dir "/etc/config.json")))
    (recovery-admin . ,recovery-admin)
    (datastore      . ,datastore)))

(define (datastore->config-file datastore)
  "Build a config.json file-like serializing the DataStore alist DATASTORE to
JSON with guile-json."
  (with-extensions (list guile-json-4)
    (with-imported-modules (source-module-closure '((json)))
      (computed-file
       "config.json"
       #~(begin
           (use-modules (json))
           (call-with-output-file #$output
             (lambda (port)
               (scm->json '#$datastore port))))))))

(define (stalwart-shepherd-service config)
  (let* ((pkg            (assoc-ref config 'package))
         (config-file    (assoc-ref config 'config-file))
         (recovery-admin (assoc-ref config 'recovery-admin))
         (extra-env      (if recovery-admin
                             (list (string-append "STALWART_RECOVERY_ADMIN="
                                                  recovery-admin))
                             '())))
    (list
     (shepherd-service
      (provision '(stalwart))
      (requirement '(user-processes networking))
      (documentation "Run the Stalwart mail and collaboration server")
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/stalwart")
                      "--config" #$config-file)
                #:user      "stalwart"
                #:group     "stalwart"
                #:log-file  "/var/log/stalwart.log"
                #:environment-variables
                (append '#$extra-env (environ))))
      (stop  #~(make-kill-destructor))
      (respawn? #t)))))

(define (stalwart-activation config)
  (let* ((data-dir    (assoc-ref config 'data-dir))
         (config-file (assoc-ref config 'config-file))
         (etc-dir     (dirname config-file))
         (datastore   (assoc-ref config 'datastore))
         (json        (and datastore (datastore->config-file datastore))))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((user (getpwnam "stalwart"))
                 (uid  (passwd:uid user))
                 (gid  (passwd:gid user)))
            (mkdir-p #$data-dir)
            (mkdir-p #$etc-dir)
            (chown #$data-dir uid gid)
            (chown #$etc-dir uid gid)
            ;; When a DataStore is declared, write a writable, stalwart-owned
            ;; config.json (bootstrap/recovery may rewrite it, so do not symlink
            ;; the read-only store file).  Otherwise leave it absent so the
            ;; server enters bootstrap mode.
            #$(if json
                  #~(begin
                      (copy-file #$json #$config-file)
                      (chmod #$config-file #o600)
                      (chown #$config-file uid gid))
                  #~#t))))))

(define (stalwart-accounts config)
  (list
   (user-group (name "stalwart") (system? #t))
   (user-account
    (name    "stalwart")
    (group   "stalwart")
    (system? #t)
    (home-directory (assoc-ref config 'data-dir))
    (shell (file-append shadow "/sbin/nologin"))
    (comment "Stalwart mail server user"))))

(define (stalwart-profile config)
  (list (assoc-ref config 'package)))

(define stalwart-service-type
  (service-type
   (name 'stalwart)
   (description "Stalwart all-in-one mail and collaboration server")
   (default-value (stalwart-configuration))
   (extensions
    (list
     (service-extension shepherd-root-service-type stalwart-shepherd-service)
     (service-extension activation-service-type    stalwart-activation)
     (service-extension account-service-type       stalwart-accounts)
     (service-extension profile-service-type       stalwart-profile)))))
