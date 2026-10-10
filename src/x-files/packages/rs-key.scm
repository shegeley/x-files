(define-module (x-files packages rs-key)
  #:use-module ((guix packages) #:select (package origin base32
                                          package-arguments
                                          package-description))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix git-download) #:select (git-fetch git-reference
                                              git-file-name))
  #:use-module (guix gexp)
  #:use-module ((guix build-system copy) #:select (copy-build-system))
  #:use-module ((guix build-system trivial) #:select (trivial-build-system))
  #:use-module ((guix utils) #:select (substitute-keyword-arguments))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages security-token) #:select (ccid
                                                       python-fido2
                                                       python-pyscard
                                                       python-yubikey-manager))
  #:use-module ((gnu packages libusb) #:select (python-hidapi python-pyusb))
  #:use-module ((gnu packages python-crypto) #:select (python-cryptography
                                                       python-shamir-mnemonic))
  #:use-module ((gnu packages finance) #:select (python-mnemonic))
  #:use-module ((gnu packages python-xyz) #:select (python-pillow))
  #:use-module ((gnu packages python) #:select (python-wrapper))
  #:use-module ((srfi srfi-13) #:select (string-drop string-join))

  #:export (ccid-rs-key
            rs-key-firmware
            rs-key-firmware-for
            rs-key-udev-rules
            rsk
            %rs-key-vendor-id
            %rs-key-product-id
            %rs-key-firmware-variants))

;; RS-Key -- an open-source hardware passkey (WebAuthn/FIDO2, ssh and git
;; signing, OpenPGP, PIV, TOTP) running Rust no_std firmware on a $5 RP2350
;; board.  https://github.com/TheMaxMur/RS-Key
;;
;; Packaged here: everything needed to USE the key on Guix System -- a CCID
;; driver that knows the device, udev rules, the `rsk' host CLI, and the
;; upstream-signed firmware images.
;;
;; NOT packaged: the firmware built from source.  That needs a
;; thumbv8m.main-none-eabihf rust-std (Guix ships no cross rust-std, so stable
;; rustc would have to go through `-Z build-std' with RUSTC_BOOTSTRAP=1 and
;; rust:rust-src), 275 crates.io deps plus 16 git-pinned embassy crates
;; vendored as individual origins, and `picotool' and `flip-link', neither of
;; which exists in Guix.  Flash `rs-key-firmware' instead: its hashes come from
;; the release's signed SHA256SUMS.

(define %rs-key-version "0.4.11")

;; The key's default USB identity: pid.codes' shared PROTOTYPE id, not an
;; allocation to the project.  The driver's reader list and the udev rules below
;; both key off this pair; firmware built with VIDPID=/USB_VID=/USB_PID=
;; presents a different one and needs neither (a Yubikey5 identity is already in
;; the stock driver and the stock yubico rules).
(define %rs-key-vendor-id "0x1209")
(define %rs-key-product-id "0x0001")

;; pcsc-lite names a reader from its USB product string, falling back to the
;; driver's friendly name; RSK_READER_TOKENS in the host CLI (tools/rsk/ccid.py)
;; matches both spellings, so this string has to stay "RS-Key".
(define %rs-key-reader-name "RS-Key")

(define ccid-rs-key
  ;; libccid binds only the USB ids in src/supported_readers.txt (generated
  ;; into the bundle's Info.plist at build time), so with the stock driver the
  ;; key's CCID interface is skipped SILENTLY: FIDO keeps working while
  ;; OpenPGP, PIV, OATH and Yubico-OTP look absent rather than broken.
  ;; Upstream cannot carry the entry either -- 0x1209:0x0001 is a shared
  ;; prototype id, and listing it in the driver would bind every unrelated
  ;; prototype using it.
  (package
    (inherit ccid)
    (name "ccid-rs-key")
    (arguments
     (substitute-keyword-arguments (package-arguments ccid)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'patch-data-paths 'add-rs-key-reader
              (lambda _
                ;; Anchor on pid.codes 0x1209's other tenant, so the entry
                ;; lands among the ids it shares a vendor with and inside the
                ;; section create_Info_plist.pl actually reads.
                (substitute* "src/supported_readers.txt"
                  (("# F-Secure Foundry")
                   (string-append
                    "# " #$%rs-key-reader-name "\n"
                    #$%rs-key-vendor-id ":" #$%rs-key-product-id ":"
                    #$%rs-key-reader-name "\n\n"
                    "# F-Secure Foundry")))))
            ;; The substitution proves the line reached the source; this proves
            ;; the Info.plist generator picked it up -- an entry parked in a
            ;; commented-out section would edit cleanly and still never bind.
            (add-after 'install 'check-reader-listed
              (lambda _
                (let* ((plist (string-append
                               #$output
                               "/pcsc/drivers/ifd-ccid.bundle/Contents/Info.plist"))
                       (text (call-with-input-file plist
                               (@ (ice-9 textual-ports) get-string-all))))
                  (unless ((@ (srfi srfi-13) string-contains)
                           text
                           (string-append "<string>" #$%rs-key-reader-name
                                          "</string>"))
                    (error "RS-Key missing from the generated Info.plist"
                           plist)))))))))
    (synopsis "PC/SC driver for USB smart cards, with RS-Key in its reader list")
    (description
     (string-append
      (package-description ccid)
      "  This variant adds the RS-Key passkey's default USB identity ("
      %rs-key-vendor-id ":" %rs-key-product-id ") to the driver's reader list,
without which @command{pcscd} skips the key's CCID interface silently and its
OpenPGP, PIV, OATH and Yubico-OTP applets appear absent rather than broken."))))

;; Firmware variant -> sha256 of its .uf2, taken from the v0.4.11 release's
;; signed SHA256SUMS.  `default' is the plain build; the rest differ only in
;; compile-time policy knobs (upstream docs/build.md): `pqc' adds ML-DSA,
;; `display' drives the trusted display, `fips' restricts the algorithm set,
;; `always-uv'/`strict-up' tighten user verification/presence, `strict-config'
;; locks configuration, `strong-pin' raises PIN policy, `2mb'/`16mb' retarget
;; the flash layout.
(define %rs-key-firmware-variants
  '(("default"        . "19fghfnix8fk5sajpl8pd5ykham7962k04f4n860rw3rns2j1y2d")
    ("pqc"            . "1dw0m4sa72999p4apv55skkhm8alv09nwbzr9hzxp1bdwavwlcy7")
    ("display"        . "19mhqynapbgjnqah8nclb8m0pv0j4ynaknwi2i4vl8gb29cwhrkx")
    ("fips"           . "0dg5pyciwpkwl4jj3wsar5693qn9icf78x53ndfxz2r2zny8c57d")
    ("fips-pqc"       . "12z96k9xb89xmm0xcj2mka0r1jgxq2crwqc43vw02fqdmgaxmhhi")
    ("always-uv"      . "1pkz4gyijvkicni05w3z7rvy4dqnqs1lxqr4l7713zfwvc3r4vyl")
    ("always-uv-pqc"  . "030p7l0m1qm3vr30g61zn0ixcjzrp8lvnj0vg6fkh52j9zr91ra8")
    ("strict-up"      . "039k992wkpi80s7wh40hhj0z9767xyzz01f8hw472mrmfgwdwh7c")
    ("strict-up-pqc"  . "1z56bnwkb6xkypc6ipnlpvmwc5i6593gg45fbch7mkii7csacw7l")
    ("strict-config"  . "1s41fwnrcxwl1f7lpy9gbnadlr8kz3icj7sw5mqa4sq8ax1zvl2w")
    ("strong-pin"     . "0ikzi56pcfxp7jv1zkrcrwfzr9s5rskwd4gz5agsfxy8m083520x")
    ("strong-pin-pqc" . "1nzail1z2vbl3mnfbh8phld0adk69qbbbyilamhibs6h58abigrq")
    ("2mb"            . "194hfd710fk7z4lzca13nbvhd1mb1lw612vrfh5l0bc9pira2rv9")
    ("16mb"           . "00gc5sp9m8mxajdl6a5p06yirf1kw5nm18kycbk1g9rvv8q5003a")))

(define* (rs-key-firmware-for #:optional (variant "default"))
  "Return the upstream-built RS-Key firmware image for @var{variant} (a key of
@code{%rs-key-firmware-variants}), installed as
@file{share/rs-key/firmware/rs-key-<variant>.uf2}: copy that onto the board's
BOOTSEL mass-storage volume to flash it.  The image is UNSIGNED -- secure boot
seals it with your own key (upstream @file{docs/production.md})."
  (let* ((hash (or (assoc-ref %rs-key-firmware-variants variant)
                   (error (string-append
                           "rs-key-firmware: unknown variant '" variant
                           "' -- known: "
                           (string-join (map car %rs-key-firmware-variants)
                                        " ")))))
         (image (origin
                  (method url-fetch)
                  (uri (string-append
                        "https://github.com/TheMaxMur/RS-Key/releases/download/v"
                        %rs-key-version "/rs-key-v" %rs-key-version
                        "-" variant ".uf2"))
                  (sha256 (base32 hash)))))
    (package
      (name (string-append "rs-key-firmware-" variant))
      (version %rs-key-version)
      (source image)
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules ((guix build utils) #:select (mkdir-p)))
            (let ((directory (string-append #$output "/share/rs-key/firmware")))
              (mkdir-p directory)
              (copy-file #$image
                         (string-append directory "/rs-key-" #$variant ".uf2"))))))
      (home-page "https://themaxmur.github.io/RS-Key/")
      (synopsis (string-append "RS-Key passkey firmware image (" variant
                               " build)"))
      (description
       "Upstream-built, @code{SHA256SUMS}-verified RS-Key firmware image in UF2
format, ready to flash onto an RP2350 board held in BOOTSEL mode.  Not built
from source -- see the note in @code{(x-files packages rs-key)}.")
      (license license:agpl3))))

(define rs-key-firmware (rs-key-firmware-for))

(define rs-key-udev-rules
  ;; The stock yubico rules (libfido2, yubikey-manager) do not cover this
  ;; VID:PID, so without these the FIDO hidraw node and the raw USB interfaces
  ;; stay root-only: `rsk', fido2-token and `ssh-keygen -t ed25519-sk' all fail
  ;; as a plain user.  pcscd reaches the CCID interface as root regardless.
  (package
    (name "rs-key-udev-rules")
    (version %rs-key-version)
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules ((guix build utils) #:select (mkdir-p)))
          (let ((directory (string-append #$output "/lib/udev/rules.d")))
            (mkdir-p directory)
            (call-with-output-file (string-append directory "/70-rs-key.rules")
              (lambda (port)
                (for-each
                 (lambda (subsystem)
                   (format port
                           "SUBSYSTEM==\"~a\", ATTRS{idVendor}==\"~a\", \
ATTRS{idProduct}==\"~a\", TAG+=\"uaccess\"~%"
                           subsystem
                           #$(string-drop %rs-key-vendor-id 2)
                           #$(string-drop %rs-key-product-id 2)))
                 '("hidraw" "usb"))))))))
    (home-page "https://themaxmur.github.io/RS-Key/")
    (synopsis "udev rules granting desktop users access to an RS-Key")
    (description
     "udev rules tagging the RS-Key passkey's hidraw and USB nodes with
@code{uaccess}, so the logged-in user may talk to its FIDO interface
(@command{fido2-token}, @code{ssh-keygen -t ed25519-sk}, python-fido2) and the
@command{rsk} CLI may reach its raw USB interfaces.")
    (license license:agpl3)))

(define rsk
  ;; tools/rsk is a plain package run as `python -m rsk' -- upstream ships no
  ;; setup.py for it, their dev shell just puts tools/ on PYTHONPATH -- so it is
  ;; installed as a module directory plus a launcher rather than through a
  ;; Python build system.
  (package
    (name "rsk")
    (version %rs-key-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TheMaxMur/RS-Key")
             (commit (string-append "v" %rs-key-version))))
       (file-name (git-file-name "rs-key" %rs-key-version))
       (sha256
        (base32 "0qmfv8glqjmpn9d3wsrrwl8gs8cwgm66wc86adkq14j44xgn6g0d"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("tools/rsk" "share/rs-key/python/rsk")
          ("docs/linux.md" "share/doc/rsk/linux.md"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'launcher
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The build environment's GUIX_PYTHONPATH already names every
              ;; python input's site-packages, so baking it into the launcher
              ;; makes `rsk' self-sufficient straight out of the store -- no
              ;; profile, no `guix shell' needed.
              (let ((bin (string-append #$output "/bin")))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/rsk")
                  (lambda (port)
                    (format port "#!~a
export PYTHONPATH=\"~a/share/rs-key/python:~a${PYTHONPATH:+:$PYTHONPATH}\"
exec ~a -m rsk \"$@\"~%"
                            (search-input-file inputs "bin/bash")
                            #$output
                            (getenv "GUIX_PYTHONPATH")
                            (search-input-file inputs "bin/python3"))))
                (chmod (string-append bin "/rsk") #o555)))))))
    (inputs
     ;; `rsk's transports and crypto: CTAPHID over hidapi, raw USB descriptors
     ;; over pyusb, PC/SC for the CCID applets, P-256 ECDH/AES/HMAC for
     ;; clientPIN and MSE backup, BIP-39/SLIP-39 seed rendering, FIDO
     ;; management, keyboard-OTP through ykman as an importable module.
     (list bash-minimal
           python-wrapper
           python-hidapi
           python-pyusb
           python-pyscard
           python-cryptography
           python-mnemonic
           python-shamir-mnemonic
           python-fido2
           python-pillow
           python-yubikey-manager))
    (home-page "https://themaxmur.github.io/RS-Key/")
    (synopsis "Host CLI for the RS-Key hardware passkey")
    (description
     "@command{rsk} is the host-side CLI for an RS-Key device: status and fleet
inventory, wallet-style seed backup and primary/backup pairing, seed lock,
secure-boot provisioning, OTP-MKEK burn and lock, FIDO management (PIN,
passkeys), LED configuration and hardware wiring, OpenPGP reset, audit,
offboarding and hands-free reboot.  Needs the key's udev rules
(@code{rs-key-udev-rules}) to run as a plain user, and @command{pcscd} with
@code{ccid-rs-key} for the smart-card applets.")
    (license license:agpl3)))
