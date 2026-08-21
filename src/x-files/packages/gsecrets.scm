(define-module (x-files packages gsecrets)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib
                                               gobject-introspection
                                               python-pygobject))
  #:use-module ((gnu packages gnome) #:select (libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk gtksourceview))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-crypto) #:select (python-pykcs11
                                                         python-pykeepass
                                                         python-pyotp
                                                         python-zxcvbn-rs-py))
  #:use-module ((gnu packages python-xyz) #:select (python-validators))
  #:use-module ((gnu packages security-token) #:select (opensc python-yubico))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Upstream (GNOME World / Secrets, aka "GSecrets") does not tag releases in
;; git; `meson.build' carries the released version number as plain data.
;; Pin to the commit checked out locally, which already declares 13.0.1.
(define %gsecrets-version "13.0.1")
(define %gsecrets-commit "18aab6b6f3fdd7e548258631e5807f94b1aceda1")

(define gsecrets-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.gnome.org/World/secrets")
          (commit %gsecrets-commit)))
    (file-name (git-file-name "gsecrets" %gsecrets-version))
    (sha256
     (base32 "197gw6w2ppg3i40n9xjw2lvcjb2vml9vs1ph022bmiarh3zb2wq9"))))

(define-public gsecrets
  (package
    (name "gsecrets")
    (version %gsecrets-version)
    (source gsecrets-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson tests shell out to pytest (not a build dependency here)
      ;; and exercise SafeGroup/SafeEntry against a live GSettings schema;
      ;; not worth wiring up for a first packaging pass.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; `gsecrets/main.py.in' bakes the exact python3 path found at
          ;; configure time straight into the installed script's shebang
          ;; (`#!@PYTHON@'), so PyGObject/GTK/pykeepass etc. must already be
          ;; on that python's search path.  `glib-or-gtk-wrap' only takes
          ;; care of GSettings-schema/typelib search paths for compiled
          ;; binaries; add PYTHONPATH ourselves the same way komikku does.
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/secrets")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))
          ;; `const.py' hardcodes PKCS11_LIB as $prefix/lib/opensc-pkcs11.so
          ;; for the hardware-key unlock provider, which never exists inside
          ;; this package's own output.  Point it at the real opensc module
          ;; instead so PKCS11/YubiKey unlock works out of the box.
          (add-after 'install 'fix-pkcs11-lib-path
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (site (car (find-files out "^const\\.py$"))))
                (substitute* site
                  (("PKCS11_LIB = .*")
                   (string-append
                    "PKCS11_LIB = \""
                    #$(file-append opensc "/lib/opensc-pkcs11.so")
                    "\"\n")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")
           gobject-introspection
           pkg-config))
    (inputs
     (list bash-minimal
           gtk
           gtksourceview
           libadwaita
           opensc
           python
           python-pygobject
           python-pykcs11
           python-pykeepass
           python-pyotp
           python-validators
           python-yubico
           python-zxcvbn-rs-py))
    (home-page "https://apps.gnome.org/Secrets/")
    (synopsis "Password and KeePass database manager for GNOME")
    (description
     "GSecrets (upstream project name @code{Secrets}) is a password manager
for GNOME, compatible with the KeePass @file{.kdbx} database format.  It can
unlock databases with a password, a key file, a hardware PKCS#11 token, or a
combination of these, generate strong passwords, and check saved entries
against Have I Been Pwned's leaked-password database.")
    (license license:gpl3+)))
