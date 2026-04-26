(define-module (x-files services clickhouse)
  #:use-module (guix gexp)
  #:use-module ((gnu services)          #:select (service-extension
                                                  service-type))
  #:use-module ((gnu services shepherd) #:select (shepherd-service
                                                  shepherd-root-service-type))
  #:use-module ((gnu services)          #:select (activation-service-type
                                                  profile-service-type))
  #:use-module ((gnu system shadow)     #:select (account-service-type
                                                  user-account
                                                  user-group))
  #:use-module (sxml simple)
  #:use-module ((gnu packages base)          #:select (tzdata))
  #:use-module ((x-files packages clickhouse) #:select (clickhouse-bin))

  #:export (clickhouse-service-type
            clickhouse-default-config))

;; Config is a plain alist.  Keys:
;;   package    — clickhouse-bin package
;;   http-port  — HTTP listen port (string, default "8123")
;;   tcp-port   — native TCP port (string, default "9000")
;;   listen     — listen host (string, default "::" for all interfaces)
;;   data-dir   — runtime data directory (default "/var/lib/clickhouse")
;;   log-dir    — log directory (default "/var/log/clickhouse-server")
;;   log-level  — log verbosity: trace|debug|information|warning|error (default "information")
;;   log-size   — max log file size before rotation (default "500M")
;;   log-count  — number of rotated log files to keep (default "3")

(define clickhouse-default-config
  `((package    . ,clickhouse-bin)
    (http-port  . "8123")
    (tcp-port   . "9000")
    (listen     . "::")
    (data-dir   . "/var/lib/clickhouse")
    (log-dir    . "/var/log/clickhouse-server")
    (log-level  . "information")
    (log-size   . "500M")
    (log-count  . "3")))

(define (cfg config key)
  (assoc-ref config key))

(define (clickhouse-accounts config)
  (list
   (user-account
    (name "clickhouse")
    (group "clickhouse")
    (system? #t)
    (home-directory (cfg config 'data-dir))
    (comment "ClickHouse server user"))
   (user-group
    (name "clickhouse"))))

(define (sxml->xml-string tree)
  "Serialize SXML TREE to an XML string."
  (call-with-output-string
    (lambda (port)
      (display "<?xml version=\"1.0\"?>\n" port)
      (sxml->xml tree port))))

(define (clickhouse-config-file config)
  "Generate a minimal config.xml for the clickhouse-server service via SXML."
  (plain-file
   "clickhouse-config.xml"
   (sxml->xml-string
    `(clickhouse
      (logger
       (level ,(cfg config 'log-level))
       (log ,(string-append (cfg config 'log-dir)
                            "/clickhouse-server.log"))
       (errorlog ,(string-append (cfg config 'log-dir)
                                 "/clickhouse-server.err.log"))
       (size ,(cfg config 'log-size))
       (count ,(cfg config 'log-count)))
      (http_port ,(cfg config 'http-port))
      (tcp_port  ,(cfg config 'tcp-port))
      (path      ,(string-append (cfg config 'data-dir) "/"))
      (tmp_path  ,(string-append (cfg config 'data-dir) "/tmp/"))
      (user_files_path
       ,(string-append (cfg config 'data-dir) "/user_files/"))
      (access_control_path
       ,(string-append (cfg config 'data-dir) "/access/"))
      (format_schema_path
       ,(string-append (cfg config 'data-dir) "/format_schemas/"))
      (users_config
       ,(string-append (cfg config 'data-dir) "/users.xml"))
      (listen_host ,(cfg config 'listen))))))

(define %clickhouse-users-xml
  (plain-file
   "clickhouse-users.xml"
   (sxml->xml-string
    '(clickhouse
      (profiles
       (default
         (max_memory_usage "10000000000")
         (use_uncompressed_cache "0")
         (load_balancing "random"))
       (readonly
        (readonly "1")))
      (users
       (default
         (password "")
         (networks (ip "::/0"))
         (profile "default")
         (quota "default")
         (access_management "1")
         (named_collection_control "1")))
      (quotas
       (default
         (interval
          (duration "3600")
          (queries "0")
          (errors "0")
          (result_rows "0")
          (read_rows "0")
          (execution_time "0"))))))))

(define (clickhouse-activation config)
  #~(begin
      (use-modules (guix build utils))
      (for-each mkdir-p
                (list #$(cfg config 'data-dir)
                      (string-append #$(cfg config 'data-dir) "/tmp")
                      (string-append #$(cfg config 'data-dir) "/user_files")
                      (string-append #$(cfg config 'data-dir) "/access")
                      (string-append #$(cfg config 'data-dir) "/format_schemas")
                      #$(cfg config 'log-dir)))
      ;; Install users.xml into the data directory (config points there)
      (copy-file #$%clickhouse-users-xml
                 (string-append #$(cfg config 'data-dir) "/users.xml"))
      (let* ((pw  (getpwnam "clickhouse"))
             (uid (passwd:uid pw))
             (gid (passwd:gid pw)))
        (for-each (lambda (dir) (chown dir uid gid))
                  (list #$(cfg config 'data-dir)
                        (string-append #$(cfg config 'data-dir) "/tmp")
                        (string-append #$(cfg config 'data-dir) "/user_files")
                        (string-append #$(cfg config 'data-dir) "/access")
                        (string-append #$(cfg config 'data-dir) "/format_schemas")
                        #$(cfg config 'log-dir)))
        (chown (string-append #$(cfg config 'data-dir) "/users.xml") uid gid))))

(define (clickhouse-shepherd-service config)
  (list
   (shepherd-service
    (provision '(clickhouse))
    (requirement '(file-systems networking))
    (documentation "Run the ClickHouse column-oriented database server.")
    (start
     #~(make-forkexec-constructor
        (list #$(file-append (cfg config 'package) "/bin/clickhouse-server")
              "--config-file" #$(clickhouse-config-file config))
        #:user  "clickhouse"
        #:group "clickhouse"
        #:environment-variables
        (list (string-append "TZDIR=" #$(file-append tzdata "/share/zoneinfo")))
        #:log-file (string-append #$(cfg config 'log-dir)
                                  "/clickhouse-shepherd.log")))
    (stop #~(make-kill-destructor))
    (auto-start? #t))))

(define-public clickhouse-service-type
  (service-type
   (name 'clickhouse)
   (description "ClickHouse column-oriented database server.")
   (extensions
    (list
     (service-extension account-service-type    clickhouse-accounts)
     (service-extension activation-service-type clickhouse-activation)
     (service-extension profile-service-type
                        (lambda (config) (list (cfg config 'package) tzdata)))
     (service-extension shepherd-root-service-type clickhouse-shepherd-service)))
   (default-value clickhouse-default-config)))
