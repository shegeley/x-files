(define-module (x-files services remark42)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu system shadow) #:select (account-service-type
                                              user-account
                                              user-group))
  #:use-module ((x-files packages remark42) #:select (remark42))

  #:export (remark42-configuration
            remark42-service-type))

(define* (remark42-configuration
          #:key
          (package    remark42)
          (url        "https://remark42.grigory.tech")
          (site       "grigory.tech")
          (secret     "change-me")
          (data-dir   "/data/remark42/grigory.tech")
          (port       8080)
          (auth-anon? #t))
  `((package    . ,package)
    (url        . ,url)
    (site       . ,site)
    (secret     . ,secret)
    (data-dir   . ,data-dir)
    (port       . ,port)
    (auth-anon? . ,auth-anon?)))

(define (remark42-shepherd-service config)
  (let* ((pkg      (assoc-ref config 'package))
         (url      (assoc-ref config 'url))
         (site     (assoc-ref config 'site))
         (secret   (assoc-ref config 'secret))
         (data-dir (assoc-ref config 'data-dir))
         (port     (assoc-ref config 'port))
         (anon?    (assoc-ref config 'auth-anon?)))
    (list
     (shepherd-service
      (provision '(remark42))
      (requirement '(user-processes file-systems networking))
      (documentation "Run the Remark42 commenting engine")
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/bin/remark42")
                      "server")
                #:user  "remark42"
                #:group "remark42"
                #:log-file "/var/log/remark42.log"
                #:environment-variables
                (list (string-append "REMARK_URL="      #$url)
                      (string-append "SITE="            #$site)
                      (string-append "SECRET="          #$secret)
                      (string-append "STORE_BOLT_PATH=" #$data-dir)
                      (string-append "BACKUP_PATH="     #$(string-append data-dir "/backup"))
                      (string-append "IMAGE_FS_PATH="   #$(string-append data-dir "/images"))
                      (string-append "AVATAR_FS_PATH="  #$(string-append data-dir "/avatars"))
                      (string-append "AUTH_ANON="       #$(if anon? "true" "false"))
                      (string-append "PORT="            #$(number->string port))
                      "SSL_TYPE=none")))
      (stop  #~(make-kill-destructor))
      (respawn? #t)))))

(define (remark42-activation config)
  (let ((data-dir (assoc-ref config 'data-dir)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((user (getpwnam "remark42"))
                 (uid  (passwd:uid user))
                 (gid  (passwd:gid user)))
            (mkdir-p #$data-dir)
            (mkdir-p #$(string-append data-dir "/backup"))
            (mkdir-p #$(string-append data-dir "/images"))
            (mkdir-p #$(string-append data-dir "/avatars"))
            (chown #$data-dir uid gid)
            (chown #$(string-append data-dir "/backup") uid gid)
            (chown #$(string-append data-dir "/images") uid gid)
            (chown #$(string-append data-dir "/avatars") uid gid))))))

(define (remark42-accounts _)
  (list
   (user-group  (name "remark42") (system? #t))
   (user-account
    (name    "remark42")
    (group   "remark42")
    (system? #t)
    (home-directory "/var/lib/remark42")
    (comment "Remark42 commenting engine"))))

(define-public remark42-service-type
  (service-type
   (name 'remark42)
   (description "Remark42 self-hosted commenting engine")
   (default-value (remark42-configuration))
   (extensions
    (list
     (service-extension shepherd-root-service-type remark42-shepherd-service)
     (service-extension activation-service-type    remark42-activation)
     (service-extension account-service-type       remark42-accounts)))))
