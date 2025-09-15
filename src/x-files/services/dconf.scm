(define-module (x-files services dconf)
  #:use-module (x-files utils records)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (guix gexp)
  #:use-module (guix modules)

  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gnome)

  #:export (dconf-home-service-type
            dconf-service-conf))

(define guile-ini:extensions
  (list guile-ini guile-smc guile-lib))

;; (define dump
;;   (with-extensions guile-ini:extensions
;;     #~(begin
;;         (use-modules
;;          (ice-9 popen)
;;          (ini))
;;         (let [(port (open-input-pipe
;;                      (string-append
;;                       #$(file-append dconf "/bin/dconf")
;;                       " " "dump" " " "/")))]
;;           (ini->scm port)))))

(define-record-type! dconf-service-conf
  ;; ini->scm result
  ;; for more info see https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.htm l
  (entries (default `())))

;; (define (envars! config)
;;   `(("DCONF_PROFILE" .
;;      ,(string-append (getenv "XDG_CONFIG_HOME")
;;                      "./dconf/profiles/main"))))

(define (serialize-ini-gexp entries)
  (with-extensions guile-ini:extensions
    #~(begin
        (use-modules (ini))
        (call-with-output-file #$output
          (lambda (port) (scm->ini #$entries #:port port))))))

(define (files config)
  (let ((entries (dconf-service-conf:entries config)))
    `(("dconf/profile.ini"
       ,(computed-file
         (basename "profile.ini")
         (serialize-ini-gexp entries))))))

(define (activation config)
  ;; TODO: make one-shot shepherd service from this script
  (let ((entries (dconf-service-conf:entries config)))
    (with-extensions guile-ini:extensions
      #~(begin
          (use-modules (ini) (ice-9 popen) (ice-9 textual-ports))

          (let [(p (open-output-pipe
                    (string-append
                     #$(file-append dconf "/bin/dconf")
                     " " "load" " " "/")))]
            (scm->ini #$entries #:port p)
            (close-port p))))))

(define-public home-dconf-service-type
  (service-type
   (name 'dconf-settings)
   (extensions
    (list
     (service-extension home-xdg-configuration-files-service-type files)
     (service-extension home-activation-service-type activation)))
   (default-value (dconf-service-conf))
   (description
    "A simple service to set all the proper dconf keys to manage Gnome desktop (Custom keyboard shortcuts, applications colorschemes and etc).
     @example
     (service home-dconf-service-type
            (dconf-service-conf
             (entries
            #~`((\"org/gnome/GPaste\"
                  (\"history-name\" . \"'history'\")
                  (\"track-extension-state\" . \"false\"))
                (\"org/gnome/shell\"
                  (\"disable-user-extensions\" . \"false\")
                  (\"disabled-extensions\" . \"['clipboard-indicator@tudmotu.com']\")
                  (\"enabled-extensions\" . \"['user-theme@gnome-shell-extensions.gcampax.github.com', 'GPaste@gnome-shell-extensions.gnome.org']\")
                  (\"remember-mount-password\" . \"true\"))
                (\"org/gnome/shell/extensions/user-theme\"
                  (\"name\" . \"'Orchis-dark'\"))
                (\"org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0\"
                  (\"binding\" . \"'<Control><Alt>t'\")
                  (\"command\" . ,(string-append \"'\" #$gnome-terminal \"/bin/gnome-terminal\" \"'\"))
                 (\"name\" . \"'Terminal'\"))
                (\"org/gnome/settings-daemon/plugins/media-keys\"
                 (\"custom-keybindings\" . \"['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/']\"))))))
     @end example
     NOTE: dconf-service-type already exists in (gnu services xorg). But it system (not home) service and too complicated to sucha little job it's should do.
    ")))

;; (define entries
;;   (let [(emacsclient (file-append emacs-pgtk "/bin/emacsclient"))]
;;     #~`(("org/gnome/GPaste"
;;          ("history-name" . "'history'")
;;          ("track-changes" . "true")
;;          ("track-extension-state" . "false"))
;;         ("org/gnome/mutter/keybindings"
;;          ;; gsettings set org.gnome.mutter.keybindings switch-monitor "[]"
;;          ;; Fucking Super+p shortcut that mess up my external monitors setup
;;          ;; I misclick it way too often
;;          ("switch-monitor" . "[]"))
;;         ("org/gnome/desktop/interface"
;;          ("color-scheme" . "'prefer-dark'")
;;          ("gtk-theme" . "'Materia-dark'")
;;          ("icon-theme" . "'Flat-Remix-Black-Dark'")
;;          ("show-battery-percentage" . "true"))
;;         ("org/gnome/desktop/input-sources"
;;          ("mru-sources" . "[('xkb', 'us'), ('xkb', 'ru')]")
;;          ("per-window" . "false")
;;          ("sources" . "[('xkb', 'us'), ('xkb', 'ru')]")
;;          ;; lv3:switch = alternate characters key (right ctrl), compose = right alt
;;          ("xkb-options" . "['grp:win_space_toggle' , 'lv3:switch', 'compose:ralt']"))
;;         ("org/gnome/desktop/sceensaver"
;;          ("color-shading-type" . "'solid'")
;;          ("lock-delay" . "uint32 0")
;;          ("lock-enabled" . "false")
;;          ("picture-options" . "'fill'")
;;          ("primary-color" . "'#3465a4'")
;;          ("secondary-color" . "'#000000'"))
;;         ("org/gnome/desktop/background"
;;          ("color-shading-type" . "'solid'")
;;          ("lock-delay" . "uint32 0")
;;          ("lock-enabled" . "false")
;;          ("picture-options" . "'fill'")
;;          ("primary-color" . "'#3465a4'")
;;          ("secondary-color" . "'#000000'"))
;;         ("org/gnome/GPaste"
;;          ("history-name" . "'history'")
;;          ("track-extension-state" . "false"))
;;         ("org/gnome/shell"
;;          ("disable-user-extensions" . "false")
;;          ("disabled-extensions" . "['clipboard-indicator@tudmotu.com']")
;;          ("enabled-extensions" . "['user-theme@gnome-shell-extensions.gcampax.github.com', 'GPaste@gnome-shell-extensions.gnome.org']")
;;          ("remember-mount-password" . "true"))
;;         ("org/gnome/settings-daemon/plugins/media-keys"
;;          ("custom-keybindings" . "['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/']"))
;;         ("org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0"
;;          ("binding" . "'<Control><Alt>t'")
;;          ("command" . ,(string-append "'" #$emacsclient " " "-c" " " "-e" " " "\"(vterm)\"" "'"))
;;          ("name" . "'Emacs Vterm'"))
;;         ("org/gnome/GPaste"
;;          ("history-name" . "'history'")
;;          ("track-extension-state" . "false")))))

;; ((@ (rde api store) build-with-store)
;;  (computed-file
;;   (basename dconf-profile)
;;   serialize-ini-gexp))
