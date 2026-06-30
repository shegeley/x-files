(define-module (x-files services greetd)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module ((gnu packages admin) #:select (greetd))
  #:use-module ((gnu packages bash)  #:select (bash))
  #:use-module ((gnu system pam)     #:select (pam-root-service-type
                                               unix-pam-service))
  #:use-module ((gnu system shadow)  #:select (account-service-type
                                               user-account
                                               user-group))
  #:export (greetd-configuration
            greetd-service-type))

;;;
;;; greetd login manager service.
;;;
;;; Guix packages the `greetd' daemon (and the `agreety' text greeter) but ships
;;; no service for it, so we provide a minimal `greetd-service-type' here.  It is
;;; the standard way (used by postmarketOS et al.) to autologin into a Wayland
;;; session such as Phosh, and it transfers unchanged to real hardware.
;;;
;;; greetd is driven by a small TOML config:
;;;
;;;   [terminal]
;;;   vt = 1
;;;   [default_session]              ; the greeter shown on boot / after logout
;;;   command = "<agreety> --cmd <session>"
;;;   user = "greeter"
;;;   [initial_session]              ; optional: log straight in once, at boot
;;;   command = "<session>"
;;;   user = "<autologin-user>"
;;;
;;; Following the channel convention, configuration is a plain alist (no
;;; records).  `session-command' is a file-like (typically a program-file that
;;; sets up the environment and execs the compositor/shell).
;;;

(define* (greetd-configuration
          #:key
          (package        greetd)
          (vt             1)
          (session-command #f)      ; file-like | string: the session to launch
          (autologin-user #f)       ; string | #f: user to log straight in
          (greeter-user   "greeter"))
  `((package         . ,package)
    (vt              . ,vt)
    (session-command . ,session-command)
    (autologin-user  . ,autologin-user)
    (greeter-user    . ,greeter-user)))

(define (greetd-config-file config)
  "Build the greetd config.toml file-like from CONFIG."
  (let* ((pkg       (assoc-ref config 'package))
         (vt        (assoc-ref config 'vt))
         (session   (assoc-ref config 'session-command))
         (autologin (assoc-ref config 'autologin-user))
         (greeter   (assoc-ref config 'greeter-user))
         (agreety   (file-append pkg "/bin/agreety")))
    (apply mixed-text-file "greetd-config.toml"
           `("[terminal]\nvt = " ,(number->string vt) "\n\n"
             "[default_session]\ncommand = \"" ,agreety " --cmd " ,session "\"\n"
             "user = \"" ,greeter "\"\n"
             ,@(if autologin
                   `("\n[initial_session]\ncommand = \"" ,session "\"\n"
                     "user = \"" ,autologin "\"\n")
                   '())))))

(define (greetd-shepherd-service config)
  (let ((pkg  (assoc-ref config 'package))
        (toml (greetd-config-file config)))
    (list
     (shepherd-service
      (provision '(greetd))
      ;; elogind gives the logind seat/session a Wayland compositor needs;
      ;; dbus-system is required by elogind/NetworkManager talking to it.
      (requirement '(user-processes elogind dbus-system))
      (documentation "greetd login manager (autologin into the Phosh session)")
      (start #~(make-forkexec-constructor
                (list #$(file-append pkg "/sbin/greetd")
                      "--config" #$toml)))
      (stop  #~(make-kill-destructor))
      (respawn? #t)))))

(define (greetd-accounts config)
  (let ((greeter (assoc-ref config 'greeter-user)))
    (list
     (user-group (name "greetd") (system? #t))
     (user-account
      (name    greeter)
      (group   "greetd")
      (system? #t)
      ;; Needs DRM/input access to run a compositor-based greeter.
      (supplementary-groups '("video" "input" "audio" "tty"))
      (home-directory "/var/lib/greetd")
      (shell   (file-append bash "/bin/bash"))
      (comment "greetd greeter user")))))

(define (greetd-pam-services config)
  ;; A standard Unix PAM service named "greetd"; elogind's own pam extension
  ;; augments it with pam_elogind so the launched session is registered with
  ;; logind.  Empty passwords are allowed for the throwaway VM autologin user.
  (list (unix-pam-service "greetd"
                          #:allow-empty-passwords? #t
                          #:login-uid? #t)))

(define (greetd-profile config)
  (list (assoc-ref config 'package)))

(define greetd-service-type
  (service-type
   (name 'greetd)
   (description "greetd login manager that autologins into a Wayland session.")
   (default-value (greetd-configuration))
   (extensions
    (list
     (service-extension shepherd-root-service-type greetd-shepherd-service)
     (service-extension account-service-type       greetd-accounts)
     (service-extension pam-root-service-type       greetd-pam-services)
     (service-extension profile-service-type        greetd-profile)))))
