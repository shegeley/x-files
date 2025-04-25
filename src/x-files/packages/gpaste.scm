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
  #:use-module ((gnu packages) #:hide (search-auxiliary-file))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public gpaste
  ;; https://patchwise.org/77780
  (package
    (name "gpaste")
    (version "45.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Keruspe/GPaste")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x8rqxqzggvzs0hr9hkwq9gmzn1m16jwllml4m4bxrdib4c9lh8q"))
              (patches
               (parameterize
                   ((%patch-path
                     (map (lambda (directory)
                            (string-append directory "/x-files/packages/patches/gpaste"))
                          %load-path)))
                 (search-patches "fix-paths.patch")))))
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
               (add-after 'unpack 'fix-introspection-install-dir
                 (lambda _
                   (substitute* "src/libgpaste/gpaste/gpaste-settings.c"
                     (("@gschemasCompiled@")
                      (string-append #$output "/share/glib-2.0/schemas/")))))
               (add-after 'install 'wrap-typelib
                 ;; absolute copy from nixpkgs
                 ;; https://github.com/NixOS/nixpkgs/blob/master/pkgs/by-name/gp/gpaste/package.nix#L67
                 (lambda _
                   (let* [(extension-dir
                           (string-append
                            #$output
                            "/share"
                            "/gnome-shell"
                            "/extensions"
                            "/GPaste@gnome-shell-extensions.gnome.org"))
                          (extension.js (string-append extension-dir "/extension.js"))
                          (extension-wrapped.js (string-append extension-dir "/.extension-wrapped.js"))
                          (prefs.js (string-append extension-dir "/prefs.js"))
                          (prefs-wrapped.js (string-append extension-dir "/.prefs-wrapped.js"))
                          (wrapper.js
                           #$(plain-file
                              "wrapper.js"
                              "import GIRepository from 'gi://GIRepository';
GIRepository.Repository.prepend_search_path('@typelibDir@');
export default (await import('./.@originalName@-wrapped.js')).default;"))
                          (typelibdir (string-append #$output "/lib/girepository-1.0"))]
                     (rename-file extension.js extension-wrapped.js)
                     (rename-file prefs.js prefs-wrapped.js)
                     (copy-file wrapper.js extension.js)
                     (copy-file wrapper.js prefs.js)
                     (substitute* extension.js
                       (("@originalName@") "extension")
                       (("@typelibDir@") typelibdir))
                     (substitute* prefs.js
                       (("@originalName@") "prefs")
                       (("@typelibDir@") typelibdir))))))))
    (home-page "https://github.com/Keruspe/GPaste")
    (synopsis "Clipboard management system for GNOME Shell")
    (description "GPaste is a clipboard manager, a tool which allows you to
keep a trace of what you’re copying and pasting.  Is is really useful when
you go through tons of documentation and you want to keep around a bunch of
functions you might want to use, for example.  The clipboard manager will
store an history of everything you do, so that you can get back to older
copies you now want to paste.")
    (license license:bsd-2)))

gpaste
