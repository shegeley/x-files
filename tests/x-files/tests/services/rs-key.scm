(define-module (x-files tests services rs-key)
  #:use-module ((gnu tests) #:select (%simple-os
                                      marionette-operating-system
                                      system-test))
  #:use-module ((gnu system) #:select (operating-system
                                       operating-system-user-services))
  #:use-module ((gnu system vm) #:select (virtual-machine))
  #:use-module ((rde features) #:select (feature-system-services-getter))
  #:use-module ((x-files features rs-key) #:select (feature-rs-key))
  #:use-module ((x-files packages rs-key) #:select (%rs-key-vendor-id))
  #:use-module (guix gexp)
  #:export (%test-rs-key))

;; What this pins down, in a booted system rather than in the package alone:
;; pcscd actually comes up with the PATCHED driver (the stock `ccid' binds no
;; 0x1209:0x0001 and skips the key's CCID interface silently), and the key's
;; udev rules reach /etc/udev/rules.d.  Both are system-level wiring that a
;; package build cannot observe.

(define %services
  ((feature-system-services-getter (feature-rs-key)) #f))

(define (run-test)
  ;; `simple-operating-system' is a macro over literal service forms, so the
  ;; feature's services are appended to %simple-os by hand instead.
  (define os
    (marionette-operating-system
     (operating-system
       (inherit %simple-os)
       (services (append %services
                         (operating-system-user-services %simple-os))))
     #:imported-modules '((gnu services herd))))

  (define vm (virtual-machine (operating-system os) (memory-size 1024)))

  (gexp->derivation
   "rs-key"
   (with-imported-modules '((gnu build marionette))
     #~(begin
         (use-modules ((gnu build marionette) #:select (make-marionette
                                                        marionette-eval
                                                        wait-for-file
                                                        system-test-runner))
                      ((srfi srfi-64) #:select (test-runner-current
                                                test-begin test-assert
                                                test-end)))

         (define marionette (make-marionette (list #$vm)))

         (define (guest expression) (marionette-eval expression marionette))

         (define (file-contains? file string)
           "Return #t when the guest's FILE contains STRING."
           (guest
            `(let ((port (open-input-file ,file))
                   (read-line (@ (ice-9 rdelim) read-line))
                   (contains? (@ (srfi srfi-13) string-contains)))
               (let loop ((line (read-line port)))
                 (cond
                  ((eof-object? line) #f)
                  ((contains? line ,string) #t)
                  (else (loop (read-line port))))))))

         (test-runner-current (system-test-runner #$output))
         (test-begin "rs-key")

         (test-assert "pcscd is running"
           (guest
            '(begin
               (use-modules (gnu services herd))
               (start-service 'pcscd))))

         ;; pcscd's activation symlinks every configured usb-driver into
         ;; /var/lib/pcsc/drivers; without ccid-rs-key there would be a bundle
         ;; here too, but its Info.plist would not name the key.
         (test-assert "the CCID driver bundle is installed for pcscd"
           (wait-for-file
            "/var/lib/pcsc/drivers/ifd-ccid.bundle/Contents/Info.plist"
            marionette))

         (test-assert "the installed driver lists the RS-Key reader"
           (file-contains?
            "/var/lib/pcsc/drivers/ifd-ccid.bundle/Contents/Info.plist"
            "RS-Key"))

         (test-assert "the driver binds the key's vendor id"
           (file-contains?
            "/var/lib/pcsc/drivers/ifd-ccid.bundle/Contents/Info.plist"
            #$%rs-key-vendor-id))

         (test-assert "the key's udev rules are installed"
           (file-contains? "/etc/udev/rules.d/70-rs-key.rules"
                           #$(substring %rs-key-vendor-id 2)))

         (test-assert "the udev rules tag the hidraw node for the desktop user"
           (file-contains? "/etc/udev/rules.d/70-rs-key.rules"
                           "SUBSYSTEM==\"hidraw\""))

         (test-end)))))

(define %test-rs-key
  (system-test
   (name "rs-key")
   (description
    "Boot a system with @code{feature-rs-key} and check that pcscd runs with a
CCID driver that actually lists the RS-Key reader, and that the key's udev
rules are installed.")
   (value (run-test))))
