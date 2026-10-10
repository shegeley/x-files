(define-module (x-files features rs-key)
  #:use-module ((rde features) #:select (feature ensure-pred))
  #:use-module ((rde predicates) #:select (list-of))
  #:use-module ((gnu services) #:select (service simple-service))
  #:use-module ((gnu services base) #:select (udev-service-type))
  #:use-module ((gnu services security-token) #:select (pcscd-service-type
                                                        pcscd-configuration))
  #:use-module ((gnu home services) #:select (home-profile-service-type))
  #:use-module ((gnu packages security-token) #:select (libfido2
                                                        python-yubikey-manager))
  #:use-module ((x-files packages rs-key) #:select (ccid-rs-key
                                                    rs-key-firmware-for
                                                    rs-key-udev-rules
                                                    rsk))

  #:export (feature-rs-key))

;; Everything an RS-Key passkey needs from the host, in one feature: the device
;; nodes a plain user may touch (udev), the smart-card path for its OpenPGP /
;; PIV / OATH / Yubico-OTP applets (pcscd with a driver that knows the device),
;; and the host tooling.  See (x-files packages rs-key) for why the stock ccid
;; driver silently ignores the key.

(define* (feature-rs-key
          #:key
          (firmware-variants '("default"))
          (pcscd? #t))
  "Support an RS-Key hardware passkey.  Tags its hidraw/USB nodes with
@code{uaccess} so FIDO works as a plain user, runs @command{pcscd} with
@code{ccid-rs-key} (unless @var{pcscd?} is false) so the CCID applets are
reachable, and puts @command{rsk}, @command{fido2-token}, @command{ykman} and
the @var{firmware-variants} images on the profile.

@var{firmware-variants} names keys of @code{%rs-key-firmware-variants}; each
installs @file{share/rs-key/firmware/rs-key-<variant>.uf2}, which is flashed by
copying it onto the board's BOOTSEL volume."
  (define f-name 'rs-key)

  (ensure-pred (list-of string?) firmware-variants)

  (define (get-system-services config)
    (append
     (list (simple-service 'rs-key-udev-rules
                           udev-service-type
                           (list rs-key-udev-rules)))
     (if pcscd?
         ;; The stock driver list has no 0x1209:0x0001, so pcscd must get the
         ;; patched driver instead of Guix's default `ccid'.
         (list (service pcscd-service-type
                        (pcscd-configuration
                         (usb-drivers (list ccid-rs-key)))))
         '())))

  (define (get-home-services config)
    (list (simple-service 'rs-key-packages
                          home-profile-service-type
                          (append (list rsk libfido2 python-yubikey-manager)
                                  (map rs-key-firmware-for
                                       firmware-variants)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (system-services-getter get-system-services)
   (home-services-getter get-home-services)))
