(define-module (x-files packages dozor)
  #:use-module ((guix licenses)          #:prefix license:)
  #:use-module ((guix packages)          #:select (package origin base32))
  #:use-module ((guix git-download)      #:select (git-fetch
                                                    git-reference
                                                    git-file-name
                                                    git-version))
  #:use-module (guix gexp)
  #:use-module ((guix build-system copy) #:select (copy-build-system))
  #:use-module ((gnu packages base)      #:select (coreutils sed grep))
  #:use-module ((gnu packages gawk)      #:select (gawk))
  #:use-module ((gnu packages bash)      #:select (bash-minimal))
  #:use-module ((gnu packages glib)      #:select (gobject-introspection
                                                    python-pygobject))
  #:use-module ((gnu packages gnome)     #:select (libadwaita))
  #:use-module ((gnu packages gtk)       #:select (gtk python-pycairo))
  #:use-module ((gnu packages polkit)    #:select (polkit))
  #:use-module ((gnu packages python)    #:select (python)))

;; Upstream ships no tags/releases and no build system at all (bare agent.py
;; + a root/sudo/systemd install.sh) -- pin to a commit like (x-files
;; packages errands).
(define %dozor-commit "2f0a61abf48e1bf43c62c3cac10be89f9135eb4a")
(define %dozor-version (git-version "0.1" "0" %dozor-commit))

(define-public dozor
  (package
    (name "dozor")
    (version %dozor-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Toxblh/dozor")
             (commit %dozor-commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yk3pwj3qi3jmyhj48bkv3wq5ha9hqway5l8mg7516s8av074h0s"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out       (assoc-ref outputs "out"))
                     (bin       (string-append out "/bin"))
                     (libexec   (string-append out "/libexec/dozor"))
                     (share     (string-append out "/share/dozor"))
                     (actions   (string-append out "/share/polkit-1/actions"))
                     (extension (string-append out "/share/gnome-shell/extensions/dozor@toxblh.ru")))
                (mkdir-p bin)
                (mkdir-p libexec)
                (mkdir-p share)
                (mkdir-p actions)
                (mkdir-p extension)

                ;; the agent itself: a PolkitAgent-registering GTK4/libadwaita UI
                (install-file "agent.py" bin)
                (rename-file (string-append bin "/agent.py")
                             (string-append bin "/dozor"))
                (chmod (string-append bin "/dozor") #o755)

                ;; PAM hooks (pam_exec.so callers -- see dozor-service-type in
                ;; (x-files services dozor)).  These run as root, from
                ;; whatever minimal PATH sudo(8)/PAM happens to hand them, so
                ;; every external command is pinned to its store path here
                ;; instead of trusted to $PATH.
                (install-file "dozor-sudo.sh" libexec)
                (substitute* (string-append libexec "/dozor-sudo.sh")
                  (("sed '")
                   (string-append #$(file-append sed "/bin/sed") " '"))
                  (("awk -v")
                   (string-append #$(file-append gawk "/bin/awk") " -v"))
                  (("stat -c %u")
                   (string-append #$(file-append coreutils "/bin/stat") " -c %u"))
                  (("date ")
                   (string-append #$(file-append coreutils "/bin/date") " "))
                  (("cat \"")
                   (string-append #$(file-append coreutils "/bin/cat") " \""))
                  (("tr '")
                   (string-append #$(file-append coreutils "/bin/tr") " '"))
                  (("base64 -w0")
                   (string-append #$(file-append coreutils "/bin/base64") " -w0"))
                  (("readlink \"")
                   (string-append #$(file-append coreutils "/bin/readlink") " \""))
                  (("chmod 0644")
                   (string-append #$(file-append coreutils "/bin/chmod") " 0644"))
                  (("mv \"")
                   (string-append #$(file-append coreutils "/bin/mv") " \""))
                  (("timeout 120 pkcheck")
                   (string-append #$(file-append coreutils "/bin/timeout") " 120 "
                                  #$(file-append polkit "/bin/pkcheck"))))
                (chmod (string-append libexec "/dozor-sudo.sh") #o555)

                (install-file "lid-open.sh" libexec)
                (substitute* (string-append libexec "/lid-open.sh")
                  (("id -u \"")
                   (string-append #$(file-append coreutils "/bin/id") " -u \""))
                  (("rm -f \"")
                   (string-append #$(file-append coreutils "/bin/rm") " -f \""))
                  (("grep -qi")
                   (string-append #$(file-append grep "/bin/grep") " -qi")))
                (chmod (string-append libexec "/lid-open.sh") #o555)

                ;; polkit action, unioned into /etc/polkit-1/actions by
                ;; polkit-service-type via dozor-service-type
                (install-file "ru.toxblh.dozor.policy" actions)

                ;; GNOME Shell extension: disables the built-in polkit agent
                ;; component so this package's agent takes over, and exposes
                ;; a window-focus DBus helper back to it
                (install-file "extension/dozor@toxblh.ru/extension.js" extension)
                (install-file "extension/dozor@toxblh.ru/metadata.json" extension)

                ;; kept for reference only: upstream's own diagnostics
                ;; collector assumes systemd (systemctl/journalctl), neither
                ;; of which exists on Guix System -- not installed on $PATH
                (install-file "contrib/dozor-report.sh" share))))
          (add-after 'install 'wrap-dozor
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/dozor")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list python))
    (inputs
     (list bash-minimal
           ;; gobject-introspection ships the hand-written cairo-1.0.gir/
           ;; typelib itself (cairo's own build doesn't); PyGObject's Gtk
           ;; overrides import that "cairo" GI namespace eagerly, same as
           ;; (x-files packages errands).
           gobject-introspection
           gtk
           libadwaita
           polkit
           python-pygobject
           python-pycairo))
    (home-page "https://github.com/Toxblh/dozor")
    (synopsis "Polkit authentication agent that shows what is asking for root")
    (description
     "Dozor is a polkit authentication agent for GNOME Shell (GTK4/libadwaita)
that, before asking for a password, shows the requesting application, the
command being run, its process ancestry, working directory and terminal, and
offers a shortcut to focus that application's window.  It supports password,
fingerprint and other polkit-registered authentication methods, and can take
over authentication for @command{sudo} as well as native polkit actions.  A
companion GNOME Shell extension disables the desktop's built-in polkit agent
so this one is used instead.  See @code{(x-files services dozor)} for the
system-level PAM/polkit wiring this package expects.")
    (license license:gpl3)))
