(define-module (x-files services dozor)
  #:use-module ((guix records) #:select (define-record-type*))
  #:use-module ((guix gexp)    #:select (file-append))
  #:use-module ((gnu services) #:select (service-type
                                        service-extension
                                        service))
  #:use-module ((gnu services dbus) #:select (polkit-service-type))
  #:use-module ((gnu system pam) #:select (pam-entry
                                          pam-service
                                          pam-service?
                                          pam-service-name
                                          pam-service-auth
                                          pam-extension
                                          pam-root-service-type))
  #:use-module ((gnu packages freedesktop) #:select (fprintd))
  #:use-module ((x-files packages dozor) #:select (dozor))
  #:export (dozor-configuration
            dozor-configuration?
            dozor-configuration-dozor
            dozor-configuration-fingerprint?
            dozor-service-type))

;;; Wires the (x-files packages dozor) polkit agent into the system:
;;;
;;;  - registers its ru.toxblh.dozor.policy action with polkit-service-type
;;;    (extends /etc/polkit-1/actions, same as fprintd-service-type does for
;;;    its own action)
;;;
;;;  - extends pam-root-service-type's "sudo" service with a `sufficient'
;;;    pam_exec.so entry that runs dozor's own sudo hook first: it asks
;;;    polkit (pkcheck --allow-user-interaction) to show Dozor's window, and
;;;    on any failure -- agent not running, ssh session, cancel, timeout --
;;;    PAM falls through to the ordinary pam_unix.so password prompt
;;;    already in that service, exactly as upstream's install.sh intends
;;;
;;;  - when FINGERPRINT? is true, also extends "polkit-1" (the PAM service
;;;    polkit's own authentication agents authenticate against) with a
;;;    lid-state gate ahead of pam_fprintd.so: skip the fingerprint reader
;;;    when the lid is closed or the agent already asked to skip it (see
;;;    lid-open.sh), then try the fingerprint before falling through to the
;;;    base password entry.  Requires fprintd-service-type to also be
;;;    enabled -- this service does not enable it itself, since not every
;;;    machine has fingerprint hardware.
;;;
;;; What this does NOT do: GNOME Shell extension enablement (dconf
;;; enabled-extensions) and starting the agent itself in the graphical
;;; session are per-user desktop concerns, not root/system ones -- wire
;;; those from a home service/rde feature instead, the way (x-files
;;; packages ntfyr) is wired up in (g-files features ntfyr).

(define-record-type* <dozor-configuration>
  dozor-configuration make-dozor-configuration
  dozor-configuration?
  (dozor        dozor-configuration-dozor
                (default dozor))
  (fingerprint? dozor-configuration-fingerprint?
                (default #f)))

(define (dozor-polkit-actions config)
  (list (dozor-configuration-dozor config)))

(define (dozor-pam-extension config)
  (let* ((dozor (dozor-configuration-dozor config))
         (sudo-entry
          (pam-entry
           (control "sufficient")
           (module "pam_exec.so")
           (arguments (list "quiet"
                            (file-append dozor "/libexec/dozor/dozor-sudo.sh")))))
         (lid-gate-entry
          (pam-entry
           (control "[success=1 default=ignore]")
           (module "pam_exec.so")
           (arguments (list "quiet"
                            (file-append dozor "/libexec/dozor/lid-open.sh")))))
         (fprintd-entry
          (pam-entry
           (control "sufficient")
           (module (file-append fprintd "/lib/security/pam_fprintd.so"))
           (arguments (list "max_tries=1" "timeout=10")))))
    (list
     (pam-extension
      (transformer
       (lambda (pam)
         (cond
          ((string=? (pam-service-name pam) "sudo")
           (pam-service
            (inherit pam)
            (auth (cons sudo-entry (pam-service-auth pam)))))
          ((and (dozor-configuration-fingerprint? config)
                (string=? (pam-service-name pam) "polkit-1"))
           (pam-service
            (inherit pam)
            (auth (cons* lid-gate-entry fprintd-entry (pam-service-auth pam)))))
          (else pam))))))))

(define-public dozor-service-type
  (service-type
   (name 'dozor)
   (extensions
    (list (service-extension polkit-service-type dozor-polkit-actions)
          (service-extension pam-root-service-type dozor-pam-extension)))
   (default-value (dozor-configuration))
   (description
    "Register the @code{ru.toxblh.dozor.sudo} polkit action and hook
@command{sudo} to try it before falling back to a normal password prompt,
so that @uref{https://github.com/Toxblh/dozor, Dozor} can show what is
asking for root before authenticating it.  Set @code{fingerprint?} to
@code{#t} to also gate @code{polkit-1}'s @code{pam_fprintd.so} behind
Dozor's lid-state check (requires @code{fprintd-service-type}).")))
