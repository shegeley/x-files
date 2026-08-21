(define-module (x-files packages wike)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib python-pygobject))
  #:use-module ((gnu packages gnome) #:select (libadwaita libsoup))
  #:use-module ((gnu packages gtk) #:select (gtk))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages webkit) #:select (webkitgtk))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Wike is a GTK4/libadwaita GNOME Wikipedia reader written in plain Python:
;; meson only installs the .py sources verbatim under
;; $pkgdatadir/wike/ and generates a `wike' launcher script
;; (src/wike.in) whose shebang is the build-time Python.  There is no
;; pyproject/setuptools step, so packaging follows the same recipe Guix
;; already uses for Komikku (another meson+PyGObject+WebKitGTK GNOME
;; app): wrap the launcher with GUIX_PYTHONPATH/GI_TYPELIB_PATH/
;; GDK_PIXBUF_MODULE_FILE picked up from `glib-or-gtk-wrap' instead of
;; going through python-build-system.
(define %wike-version "3.2.1")
(define %wike-commit "b9ebebcefebdf04642415aba0fa4763e5a75b6d0")

(define-public wike
  (package
    (name "wike")
    (version %wike-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hugolabe/Wike")
             (commit %wike-commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "070bkbxvwbnmhdj6kdnv4vl4j2l14y0x0l3ixwjcbi8sigbzx6d8"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/wike")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                `("GDK_PIXBUF_MODULE_FILE" =
                  (,(getenv "GDK_PIXBUF_MODULE_FILE")))))))))
    (native-inputs
     (list desktop-file-utils                 ;desktop-file validate test
           gettext-minimal
           `(,glib "bin")                      ;glib-compile-resources/-schemas
           `(,gtk "bin")                        ;gtk4-update-icon-cache
           pkg-config))
    (inputs
     (list bash-minimal                        ;for wrap-program
           gtk
           libadwaita
           libsoup                              ;gi.repository.Soup, used directly
           python
           python-pygobject
           webkitgtk))                          ;gi.repository.WebKit, the reading surface
    (home-page "https://github.com/hugolabe/Wike")
    (synopsis "Wikipedia reader for GNOME")
    (description
     "Wike is a GNOME application to search and read Wikipedia articles.
It supports multiple open articles as tabs, article search with
suggestions, a table of contents panel, language links, a history and
bookmarks panel, and dark/sepia reading themes.  Articles are rendered
in an embedded @code{WebKitGTK} view.")
    (license license:gpl3+)))
