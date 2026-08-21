(define-module (x-files packages commit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages freedesktop) #:select (appstream
                                                       desktop-file-utils
                                                       libportal))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler
                                                gjs
                                                libadwaita
                                                libspelling))
  #:use-module ((gnu packages gtk) #:select (gtk gtksourceview))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Commit's build (src/meson.build) bundles the app into a single
;; gresource via `gjspack', a small gjs-based bundler that lives in the
;; `troll' git submodule.  Upstream ships gjspack pre-built (its own
;; gresource blob is committed to the troll repository), so pulling the
;; submodule in via `recursive?' is enough -- no bootstrapping of
;; gjspack's own JS toolchain is required.
(define %commit-commit "22d182bbf79ee13d829b4bce937a977a61400a87")

(define-public commit
  (package
    (name "commit")
    (version "4.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sonnyp/Commit")
             (commit %commit-commit)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m6dpjwmpqag4xmkc0pa29zdrhf6v6a4bbmz59wz0aykmg542b1z"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-env-dash-s-shebangs
            (lambda _
              ;; `#!/usr/bin/env -S gjs -m' is more than the kernel's own
              ;; shebang loader can parse (it only ever takes a single
              ;; argument after the interpreter), and Guix's
              ;; patch-shebangs phase does not special-case `env -S'
              ;; either -- it leaves the literal `-S' behind as an
              ;; unresolvable "interpreter".  Both `gjspack' (the
              ;; bundler, run at build time from the `troll' submodule)
              ;; and the app's own launcher (src/bin.js, installed as
              ;; re.sonny.Commit) use this form; rewrite both to a
              ;; plain, single-argument shebang gjs can actually run.
              (substitute* '("src/bin.js" "troll/gjspack/bin/gjspack")
                (("^#!/usr/bin/env -S gjs -m")
                 (string-append "#!" #$(file-append gjs "/bin/gjs") " -m")))))
          (add-after 'install 'wrap-gjs
            (lambda* (#:key outputs #:allow-other-keys)
              ;; The launcher (src/bin.js, installed as re.sonny.Commit)
              ;; needs every GI typelib the app touches (Gtk, Adwaita,
              ;; GtkSourceView, Spelling, Xdp) on GI_TYPELIB_PATH, which
              ;; meson's own glib-or-gtk wrap phase does not provide for
              ;; a script-only (no ELF binary) app.
              (wrap-program (string-append (assoc-ref outputs "out")
                                            "/bin/re.sonny.Commit")
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list appstream
           blueprint-compiler
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")))
    (inputs
     (list bash-minimal              ;for wrap-program
           gjs
           gtk
           gtksourceview
           libadwaita
           libportal
           libspelling))
    (home-page "https://apps.gnome.org/Commit/")
    (synopsis "Git commit message editor")
    (description
     "Commit is a small GTK4/libadwaita application dedicated to writing
Git commit messages.  It is meant to be invoked as the @code{$EDITOR} (or
@code{GIT_EDITOR}) for @command{git commit}, and offers a distraction-free
GtkSourceView editor with commit-message syntax highlighting, spell
checking, and a character/line count for the summary line.")
    (license license:gpl3+)))
