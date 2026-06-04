(define-module (x-files packages gpaste)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public gpaste
  ;; https://patchwise.org/77780
  (package
    (name "gpaste")
    (version "45.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Keruspe/GPaste")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12dkzlv5mcyanxl23lxljzdzjzf52wlii9yx54jmiinnpnh1yj2i"))))
    (build-system meson-build-system)
    (native-inputs
     (list gcr
           gettext-minimal
           gobject-introspection
           (list glib "bin")            ; for glib-compile-resources
           pkg-config
           vala))
    (inputs
     (list appstream-glib
           desktop-file-utils           ; for update-desktop-database
           gjs
           gtk+
           mutter
           libadwaita))
    (arguments
     (list #:glib-or-gtk? #true
           #:configure-flags
           #~(list
              (string-append "-Dcontrol-center-keybindings-dir="
                             #$output "/share/gnome-control-center/keybindings")
              (string-append "-Ddbus-services-dir="
                             #$output "/share/dbus-1/services")
              (string-append "-Dsystemd-user-unit-dir="
                             #$output "/etc/systemd/user"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'fix-typelib-path
                 (lambda _
                   (use-modules (ice-9 textual-ports))
                   (let* [(ext-dir
                           (string-append
                            #$output "/share/gnome-shell/extensions/"
                            "GPaste@gnome-shell-extensions.gnome.org"))
                          (typelib-dir
                           (string-append #$output "/lib/girepository-1.0"))
                          (preamble
                           (string-append
                            "import GIRepository from 'gi://GIRepository';\n"
                            "GIRepository.Repository.prepend_search_path('"
                            typelib-dir "');\n"))]
                     (for-each
                      (lambda (f)
                        (let* [(path (string-append ext-dir "/" f))
                               (content (call-with-input-file path get-string-all))]
                          (call-with-output-file path
                            (lambda (port) (display preamble port)
                                           (display content port)))))
                      '("extension.js" "prefs.js"))))))))
    (home-page "https://github.com/Keruspe/GPaste")
    (synopsis "Clipboard management system for GNOME Shell")
    (description "GPaste is a clipboard manager, a tool which allows you to
keep a trace of what you're copying and pasting.  Is is really useful when
you go through tons of documentation and you want to keep around a bunch of
functions you might want to use, for example.  The clipboard manager will
store an history of everything you do, so that you can get back to older
copies you now want to paste.")
    (license license:bsd-2)))
