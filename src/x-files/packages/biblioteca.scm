(define-module (x-files packages biblioteca)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils
                                                      libportal))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib))
  #:use-module ((gnu packages gnome) #:select (blueprint-compiler
                                                gjs
                                                libadwaita))
  #:use-module ((gnu packages gtk) #:select (gtk))
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages webkit) #:select (webkitgtk))
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; workbenchdev/Biblioteca has no release tarballs; pin to the exact commit
;; checked out in the reference clone.  meson.build's own version string
;; ('1.6') lags the "v1.7" tag at this commit -- an upstream inconsistency,
;; not a packaging bug.
(define %biblioteca-version "1.7")
(define %biblioteca-commit "e4185cfa788d9112e6780f9780c1a790ee4b1847")

;; Biblioteca vendors its JS "framework" (gsx/ThemeSelector/persistWindowState
;; etc.) as the `troll' git submodule rather than an npm dependency.  Fetch it
;; recursively; nothing here needs `npm install' or network access -- see the
;; comment on GJSPACK below.
(define biblioteca-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/workbenchdev/Biblioteca")
          (commit %biblioteca-commit)
          (recursive? #t)))
    (file-name (git-file-name "biblioteca" %biblioteca-version))
    (sha256
     (base32 "0mpcwd34vccdh8g5hgqqk280m4pqs9g73hjm70qh3w0zvpiby69x"))))

(define-public biblioteca
  (package
    (name "biblioteca")
    (version %biblioteca-version)
    (source biblioteca-source)
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags #~(list "-Dprofile=default")
      #:phases
      #~(modify-phases %standard-phases
          ;; `troll/gjspack' is a JS bundler that ships its own compiled
          ;; `gjspack.src.gresource' *inside the git tree* (see
          ;; troll/gjspack/bin/), so bundling `src/main.js' at build time
          ;; needs nothing beyond `gjs' + `blueprint-compiler' (gjspack
          ;; shells out to the latter for every `*.blp' template it meets).
          ;; There is no TypeScript, no npm install, and no network fetch
          ;; anywhere in this build -- package.json's dependencies
          ;; (eslint/prettier/husky) are dev-only lint tooling, unused here.
          ;;
          ;; Both scripts below carry a checked-in `#!/usr/bin/env -S gjs -m'
          ;; shebang; Guix build containers have no /usr/bin/env, and (as of
          ;; this Guix, see gnome-shell-extension-gsconnect for precedent)
          ;; `patch-shebangs' doesn't yet rewrite `env -S' shebangs anyway
          ;; (bug#74450).  `gjspack' is invoked straight out of the checkout
          ;; during the 'build phase and `build-index.js' is run by
          ;; `meson.add_install_script' -- both before the automatic
          ;; shebang-patching pass over the install tree even would apply.
          ;; Patch both by hand.
          (add-after 'unpack 'patch-gjs-shebangs
            (lambda _
              (for-each
               (lambda (file)
                 (substitute* file
                   (("^#!/usr/bin/env -S gjs.*$")
                    (string-append "#!" (which "gjs") " -m"))))
               '("troll/gjspack/bin/gjspack"
                 "build-aux/build-index.js"))))
          (add-after 'unpack 'tolerate-missing-flatpak-docs
            (lambda _
              ;; meson.add_install_script runs build-aux/build-index.js,
              ;; which unconditionally scans /app/share/doc for gi-docgen
              ;; trees to build data/doc-index.json.  That path only exists
              ;; inside a Flatpak sandbox, populated by the org.gnome.Sdk.Docs
              ;; SDK extension; outside Flatpak the scan throws and aborts
              ;; `ninja install' entirely.  Make the scan tolerate a missing
              ;; directory so install completes; the resulting package ships
              ;; an EMPTY doc-index.json (Guix has no gi-docgen doc trees in
              ;; the JSON+HTML layout Biblioteca expects to vendor here).
              ;; This does not address the separate, deliberate
              ;; `Xdp.Portal.running_under_flatpak()' guard in src/bin.js,
              ;; which still refuses to run the binary at all outside
              ;; Flatpak -- see the package comment below.
              (substitute* "build-aux/build-index.js"
                (("await scanLibraries\\(Gio\\.File\\.new_for_path\\(\"/app/share/doc\"\\)\\);")
                 (string-append
                  "try {\n"
                  "    await scanLibraries(Gio.File.new_for_path(\"/app/share/doc\"));\n"
                  "  } catch (e) {\n"
                  "    print(\"guix: /app/share/doc unavailable outside Flatpak; "
                  "shipping an empty doc-index.json\");\n"
                  "  }")))))
          ;; Same rationale as `mission-center': meson's post_install bakes a
          ;; per-package icon-theme.cache with a 1970 mtime, which stops GTK
          ;; from scanning the directory and blocks the profile hook from
          ;; merging a correct cache.
          (add-after 'install 'delete-icon-cache
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((cache (string-append (assoc-ref outputs "out")
                                          "/share/icons/hicolor/icon-theme.cache")))
                (when (file-exists? cache)
                  (delete-file cache)))))
          ;; `glib-or-gtk-wrap' derives GI_TYPELIB_PATH by scanning each
          ;; installed binary's ELF NEEDED entries for a GTK/GLib match; both
          ;; installed commands here are gjs/bash scripts, not ELF, so that
          ;; heuristic finds nothing to wrap and leaves GI_TYPELIB_PATH unset.
          ;; Wrap explicitly with whatever GI_TYPELIB_PATH this build itself
          ;; resolved from gtk/libadwaita/webkitgtk/libportal, mirroring
          ;; gnome-shell-extension-gsconnect's `wrap-programs' phase.
          (add-after 'glib-or-gtk-wrap 'wrap-gi-typelib-path
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (for-each
                 (lambda (program)
                   (wrap-program (string-append bin "/" program)
                     `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                 '("app.drey.Biblioteca" "biblioteca"))))))))
    (native-inputs
     (list blueprint-compiler
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gjs
           `(,gtk "bin")))
    (inputs
     (list bash-minimal
           gjs
           gtk
           libadwaita
           libportal
           webkitgtk))
    (home-page "https://github.com/workbenchdev/Biblioteca")
    (synopsis "Offline GNOME API documentation browser")
    (description
     "Biblioteca lets you browse and read GNOME API documentation offline.
It is a GJS/GTK4/libadwaita application from the Workbench team.

@b{This Guix package is severely limited compared to upstream's intended
Flatpak distribution.}  Biblioteca's install step generates its
documentation index by scanning @file{/app/share/doc}, a path populated
only inside a Flatpak sandbox by the @code{org.gnome.Sdk.Docs} SDK
extension; this package patches that scan to tolerate the path's absence,
so it ships with an @b{empty} documentation index rather than failing to
build.  Separately and more fundamentally, @file{src/bin.js} unconditionally
calls @code{Xdp.Portal.running_under_flatpak()} and exits immediately with
an error when false -- upstream's own message: \"Biblioteca is only meant
to be run sandboxed in a specific target environment.  Bypassing this will
expose users to arbitrary code breakage.\"  This package does not patch
that check out, so the installed @command{biblioteca} binary will refuse
to run at all outside of Flatpak.  Packaging a genuinely usable, native
build would additionally require vendoring gi-docgen-format documentation
trees for the relevant GNOME libraries and making a deliberate call to
override upstream's sandboxing guard.")
    (license license:gpl3)))
