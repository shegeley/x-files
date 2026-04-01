(define-module (x-files features datomic)
  #:use-module (guix gexp)
  #:use-module ((gnu services) #:select (service simple-service activation-service-type))
  #:use-module (rde features)
  #:use-module ((x-files services datomic) #:select (datomic-postgres-transactor-service-type))

  #:export (feature-datomic-postgres))

(define* (feature-datomic-postgres
          #:key
          password-file              ;; file-like object (e.g. local-file)
          (password-path "/etc/datomic-postgres-password")
          (port          "4335")
          (log-dir       "/var/log/datomic")
          (sql-url       "jdbc:postgresql://localhost:5432/datomic")
          (sql-user      "datomic"))

  (define password-activation
    #~(begin
        (copy-file #$password-file #$password-path)
        (chmod #$password-path #o444)))

  (define (get-system-services config)
    (list
     (simple-service 'datomic-password-file
                     activation-service-type
                     password-activation)
     (service datomic-postgres-transactor-service-type
              `((port              . ,port)
                (log-dir           . ,log-dir)
                (sql-url           . ,sql-url)
                (sql-user          . ,sql-user)
                (sql-password-file . ,password-path)))))

  (feature
   (name 'datomic-postgres)
   (values '())
   (system-services-getter get-system-services)))
