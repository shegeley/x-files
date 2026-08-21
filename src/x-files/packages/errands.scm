(define-module (x-files packages errands)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages freedesktop) #:select (desktop-file-utils
                                                       libportal))
  #:use-module ((gnu packages gettext) #:select (gettext-minimal))
  #:use-module ((gnu packages glib) #:select (glib
                                               gobject-introspection
                                               python-pygobject))
  #:use-module ((gnu packages gnome) #:select (libadwaita libsecret))
  #:use-module ((gnu packages gtk) #:select (gtk gtksourceview))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages python) #:select (python))
  #:use-module ((gnu packages python-build) #:select (python-hatch-vcs
                                                        python-hatchling
                                                        python-setuptools))
  #:use-module ((gnu packages python-web) #:select (python-requests))
  #:use-module ((gnu packages python-xyz) #:select (python-click
                                                      python-icalendar))
  #:use-module ((gnu packages time) #:select (python-dateutil
                                               python-tzdata))
  #:use-module ((gnu packages xml) #:select (python-lxml))
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Errands' own runtime dependency (caldav sync) pulls in two small PyPI
;; libraries that aren't packaged in Guix yet: x-wr-timezone and
;; recurring-ical-events.  Defined here, in dependency order, since nothing
;; else in the channel needs them.

(define-public python-x-wr-timezone
  (package
    (name "python-x-wr-timezone")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "x_wr_timezone" version))
       (sha256
        (base32 "1hy8bil5fdvrjiv0121z8yxw3zrc3qd4wddwpbg0wk7xdw7c8rli"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Test suite compares against live Google-Calendar-shaped fixtures
      ;; and pulls in pytz; not worth the extra native-inputs here.
      #:tests? #f))
    ;; No pyproject.toml (plain setup.py/setup.cfg): the implicit
    ;; setuptools.build_meta backend still needs setuptools on hand.
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-click python-icalendar python-tzdata))
    (home-page "https://github.com/niccokunzmann/x-wr-timezone")
    (synopsis "Repair X-WR-TIMEZONE iCalendar files to RFC 5545")
    (description
     "This library and command-line tool rewrite ICS/iCalendar files that
use the non-standard @code{X-WR-TIMEZONE} property (as produced by Google
Calendar exports) into RFC 5545-compliant files with proper
@code{VTIMEZONE} components.")
    ;; setup.py declares license='LGPL-3.0-or-later'.
    (license license:lgpl3+)))

(define-public python-recurring-ical-events
  (package
    (name "python-recurring-ical-events")
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "recurring_ical_events" version))
       (sha256
        (base32 "013xrrmsd1shnp8kyv93w5rdjq3w8x8svbwimfkmd2dxv4spr31y"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f))                    ;pulls in pytest-cov, pytz, network
    (native-inputs
     (list python-hatch-vcs python-hatchling))
    (propagated-inputs
     (list python-dateutil python-icalendar python-tzdata
           python-x-wr-timezone))
    (home-page "https://recurring-ical-events.readthedocs.io/")
    (synopsis "Compute recurrence times of iCalendar events")
    (description
     "This library expands recurring events, todos, alarms and journals
found in an iCalendar (RFC 5545) file or string into concrete occurrences
within a given time span.")
    ;; pyproject.toml's SPDX string says GPL-3.0-or-later, but the shipped
    ;; LICENSE file is the LGPLv3 text; trust the actual license text.
    (license license:lgpl3+)))

(define-public python-caldav
  (package
    (name "python-caldav")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       ;; The PyPI sdist for this release accidentally bundles the
       ;; maintainer's dev venv (10+ MiB of vendored wheels); fetch the
       ;; tagged git tree instead, which only contains the library.
       (uri (git-reference
             (url "https://github.com/python-caldav/caldav")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yp0fgj848ifkn6b4fpzybycrd1n6ngf1dxlmpiagsz02m64mdlz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; The test suite needs a live (or containerized) CalDAV server plus
      ;; vobject/tzlocal/xandikos/radicale; out of scope for this build.
      #:tests? #f))
    (native-inputs
     (list python-hatch-vcs python-hatchling))
    (propagated-inputs
     (list python-icalendar python-lxml python-recurring-ical-events
           python-requests))
    (home-page "https://github.com/python-caldav/caldav")
    (synopsis "CalDAV (RFC 4791) client library")
    (description
     "This library implements the client side of the CalDAV protocol, used
to talk to calendar servers such as Radicale, Nextcloud, Google Calendar and
iCloud.")
    ;; Dual-licensed; upstream ships both COPYING.APACHE and COPYING.GPL
    ;; (GPLv3 text, no "or later" grant in the project's own notices).
    (license (list license:asl2.0 license:gpl3))))

(define %errands-commit "79dd1694e8055814c6f7e7a41e680967a5782b21")
(define %errands-version (git-version "46.2.10" "1" %errands-commit))

(define-public errands
  (package
    (name "errands")
    (version %errands-version)
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrvladus/Errands")
             (commit %errands-commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j4y1q0916hjyn7l3n5dy4724bchvs80h0w4h68c02p3392mz5sv"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; The only meson tests are appstream/desktop/gschema validators
      ;; (invoked only when their tools are found); nothing exercises the
      ;; Python code itself.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; errands.py's shebang is a bare `#!@PYTHON@' baked at configure
          ;; time; it has no idea where its GObject-Introspection typelibs
          ;; or its Python dependencies (caldav, pygobject, ...) live once
          ;; installed.  Capture the same search paths the build environment
          ;; already assembled from our inputs and bake them into a wrapper.
          (add-after 'glib-or-gtk-wrap 'wrap-python
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/errands")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           `(,gtk "bin")
           pkg-config
           python))
    (inputs
     (list bash-minimal
           ;; gobject-introspection ships the hand-written cairo-1.0.gir/
           ;; typelib itself (cairo's own build doesn't); PyGObject's Gtk
           ;; overrides import that "cairo" GI namespace eagerly.
           gobject-introspection
           gtk
           gtksourceview
           libadwaita
           libportal
           libsecret
           python-caldav
           python-pygobject
           python-requests))
    (home-page "https://github.com/mrvladus/Errands")
    (synopsis "To-do list application for GNOME")
    (description
     "Errands is a task manager for the GNOME desktop.  It supports task
lists with sub-tasks, tags, due dates and notes, drag-and-drop reordering,
and syncing with any CalDAV-compatible server (Nextcloud, Radicale, and
others) via the caldav library.")
    (license license:expat)))
