(define-module (x-files packages iotas)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages enchant) #:select (python-pyenchant))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib
                                              gobject-introspection
                                              python-pygobject))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk gtksourceview python-pycairo))
  #:use-module ((gnu packages haskell-xyz) #:select (pandoc))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-build) #:select (python-packaging
                                                       python-poetry-core))
  #:use-module ((gnu packages python-web) #:select (python-requests))
  #:use-module ((gnu packages python-xyz) #:select (python-linkify-it-py
                                                     python-markdown-it-py
                                                     python-mdit-py-plugins
                                                     python-pypandoc))
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Iotas' spellchecking widget hard-imports `gtkspellcheck' (editor_text_view.py),
;; not wrapped in a try/except, so it's a real runtime dependency, not optional.
;; The PyPI package "pygtkspellcheck" isn't packaged in Guix yet; its own
;; dependencies (pyenchant, PyGObject) already are, so package it here rather
;; than blocking on it.
(define-public python-pygtkspellcheck
  (package
    (name "python-pygtkspellcheck")
    (version "5.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygtkspellcheck" version))
       (sha256
        (base32 "1qqc53bcy4q5mli8k43d3d1a1mn99x7yhz8ynsv610q4vvzdx4x6"))))
    (build-system pyproject-build-system)
    ;; The sdist ships no tests/ directory; pyproject-build-system's default
    ;; unittest discovery finds nothing and treats that as a failure.
    (arguments (list #:tests? #f))
    (native-inputs (list python-poetry-core))
    (propagated-inputs (list python-pyenchant python-pygobject))
    (home-page "https://github.com/koehlma/pygtkspellcheck")
    (synopsis "Spellchecking library for GTK written in pure Python")
    (description
     "@code{gtkspellcheck} is a spellchecking library for GTK text widgets,
using Enchant as its spell-checking backend.")
    (license license:gpl3+)))

(define %iotas-commit "6b69a7e275765b178227acde388b6d8369856b74")
;; meson.build has no release tag at this commit; snapshot off the version
;; the upstream project.build file itself declares.
(define %iotas-version (git-version "2026.8" "0" %iotas-commit))

(define iotas-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.gnome.org/World/iotas.git")
          (commit %iotas-commit)))
    (file-name (git-file-name "iotas" %iotas-version))
    (sha256
     (base32 "19w4fvanfcpbkck0w98f6r4adnjiv45b3d0673sw34x4l620w2v0"))))

(define-public iotas
  (package
    (name "iotas")
    (version %iotas-version)
    (source iotas-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The meson test suite is pytest plus appstream/desktop/gschema
      ;; validation; pytest needs a D-Bus session and other runtime services
      ;; the build sandbox doesn't provide.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-runtime
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; meson only bakes @PYTHON@ shebangs; neither GTK's typelibs,
              ;; the Python library dependencies, nor the `pandoc' CLI (used
              ;; by pypandoc for exporting) are found without an explicit
              ;; PATH/GI_TYPELIB_PATH/GUIX_PYTHONPATH wrapper.  The main
              ;; `iotas' binary and the D-Bus-activated search-provider
              ;; helper both need it.
              (let* ((out (assoc-ref outputs "out"))
                     (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                     (python-path (getenv "GUIX_PYTHONPATH"))
                     (pandoc-bin (string-append
                                  (assoc-ref inputs "pandoc") "/bin")))
                (for-each
                 (lambda (program)
                   (wrap-program program
                     `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                     `("GUIX_PYTHONPATH" ":" prefix (,python-path))
                     `("PATH" ":" prefix (,pandoc-bin))))
                 (list (string-append out "/bin/iotas")
                       (string-append out "/libexec/iotas-search-provider"))))))
          (add-after 'install 'delete-icon-cache
            (lambda* (#:key outputs #:allow-other-keys)
              ;; meson's post_install bakes a per-package icon-theme.cache
              ;; with a 1970 mtime, which stops GTK from scanning the
              ;; directory and blocks the profile hook from merging a
              ;; correct cache.  Drop it.
              (let ((cache (string-append (assoc-ref outputs "out")
                                          "/share/icons/hicolor/icon-theme.cache")))
                (when (file-exists? cache)
                  (delete-file cache))))))))
    (native-inputs
     (list blueprint-compiler
           desktop-file-utils           ;update-desktop-database, at install time
           gettext-minimal
           `(,glib "bin")               ;glib-compile-schemas, glib-compile-resources
           gobject-introspection        ;checked via pkg-config at configure time
           `(,gtk "bin")                ;gtk-update-icon-cache
           pkg-config
           python))                     ;found via meson's python.find_installation()
    (inputs
     (list bash-minimal                 ;used by wrap-program
           glib
           gtk
           gtksourceview
           libadwaita
           pandoc                       ;CLI used by pypandoc for note export
           python
           python-linkify-it-py
           python-markdown-it-py
           python-mdit-py-plugins
           python-packaging
           python-pycairo
           python-pygobject
           python-pygtkspellcheck
           python-pypandoc
           python-requests))
    (home-page "https://gitlab.gnome.org/World/iotas")
    (synopsis "Simple GNOME note-taking app with Nextcloud Notes sync")
    (description
     "Iotas is a note-taking application for GNOME, written with GTK4 and
libadwaita.  It supports Markdown editing with live preview, categorising
notes, exporting to several document formats, and two-way synchronisation
with a Nextcloud Notes server.")
    (license license:gpl3+)))
