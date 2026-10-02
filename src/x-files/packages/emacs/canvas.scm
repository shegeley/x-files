(define-module (x-files packages emacs canvas)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix git-download)
                #:select (git-fetch git-reference git-version git-file-name))
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module (guix gexp)
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages compression) #:select (unzip))
  #:use-module ((gnu packages emacs) #:select (emacs-next-pgtk))
  #:use-module ((gnu packages emacs-xyz)
                #:select (emacs-transient emacs-websocket))
  #:use-module ((gnu packages fonts) #:select (font-dejavu))
  #:use-module ((gnu packages gnome) #:select (librsvg))
  #:use-module ((gnu packages gtk) #:select (cairo pango gdk-pixbuf))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages xorg) #:select (xorg-server)))

(define-public emacs-canvas-keys
  (let ((commit "510c5fbc5b8d402a9b717ccd99f248c1b935eccf"))
    (package
      (name "emacs-canvas-keys")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Daskeladden/canvas-keys")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1v5kj6xxnk7zklym8a3vns7c3a6ww6d5mk266kh6k89rlm5mpl7g"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:emacs emacs-next-pgtk
        #:test-command
        #~(list "emacs" "-Q" "--batch" "-L" "." "-L" "tests"
                "-l" "tests/canvas-keys-tests.el"
                "-l" "tests/canvas-keys-headers-tests.el"
                "-f" "ert-run-tests-batch-and-exit")))
      (propagated-inputs (list emacs-transient))
      (home-page "https://github.com/Daskeladden/canvas-keys")
      (synopsis "Shared key bindings and menus for Emacs canvas buffers")
      (description
       "Canvas Keys provides consistent key maps, Transient menus, and helpers
for packages that display Emacs 32 @code{canvas} images.")
      (license gpl3+))))

(define-public emacs-canvas-diagram
  (let ((commit "3d2b4a0618a5276174ddcf80a08b94c18f5eaa16"))
    (package
      (name "emacs-canvas-diagram")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Daskeladden/canvas-diagram")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0kfq51wayafzh19pcfklz49vviwm0xjlcjblxlghp28h4v6ld3yq"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:emacs emacs-next-pgtk
        #:include #~(cons "^canvas-cairo\\.so$" %default-include)
        #:test-command
        #~(list "emacs" "-Q" "--batch" "-L" "." "-L" "tests"
                "-l" "tests/canvas-diagram-tests.el"
                "-l" "tests/canvas-palette-tests.el"
                "-l" "tests/canvas-diagram-reader-tests.el"
                "-f" "ert-run-tests-batch-and-exit")
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'check 'build-canvas-module
              (lambda _
                (substitute* "Makefile"
                  (("/usr/local/include")
                   (string-append #$emacs-next-pgtk "/include")))
                (invoke "make")))
            (add-before 'check 'prepare-fonts
              (lambda _
                (setenv "HOME" (getcwd))
                (setenv "XDG_DATA_HOME"
                        (string-append #$font-dejavu "/share")))))))
      (native-inputs (list gcc-toolchain pkg-config font-dejavu))
      (inputs (list cairo pango librsvg gdk-pixbuf))
      (propagated-inputs (list emacs-canvas-keys emacs-transient))
      (home-page "https://github.com/Daskeladden/canvas-diagram")
      (synopsis "Draw diagrams on Emacs canvas images with Cairo and Pango")
      (description
       "Canvas Diagram provides diagram rendering, navigation, palettes, and
export for Emacs 32 @code{canvas} images.  Its native @file{canvas-cairo.so}
module uses Cairo and Pango, with SVG icons and PNG, JPEG, and GIF pictures
through librsvg and GdkPixbuf.")
      (license gpl3+))))

(define-public emacs-canvas-browser
  (let ((commit "f3c7ea997d04bfd2a82e16143c23c9aa627b99d3"))
    (package
      (name "emacs-canvas-browser")
      (version (git-version "0.1.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Daskeladden/canvas-browser")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n6w0z6vwpdj4xbs6dni3x6j9h3v4n9pw08ygp8f3n3dq40a6zsr"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:emacs emacs-next-pgtk
        #:test-command
        #~(list "emacs" "-Q" "--batch" "-L" "." "-L" "tests"
                "-l" "tests/canvas-browser-cdp-tests.el"
                "-l" "tests/canvas-browser-tests.el"
                "-f" "ert-run-tests-batch-and-exit")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-programs
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* '("canvas-browser-cdp.el"
                               "tests/canvas-browser-cdp-tests.el")
                  (("\"Xvfb\"")
                   (string-append "\""
                                  (search-input-file inputs "/bin/Xvfb")
                                  "\""))
                  (("(sudo )?snap install chromium")
                   "guix install ungoogled-chromium"))
                (substitute* '("canvas-browser.el"
                               "tests/canvas-browser-tests.el")
                  (("\"unzip\"")
                   (string-append "\""
                                  (search-input-file inputs "/bin/unzip")
                                  "\"")))))
            (add-before 'check 'prepare-fonts
              (lambda _
                (setenv "HOME" (getcwd))
                (setenv "XDG_DATA_HOME"
                        (string-append #$font-dejavu "/share")))))))
      (native-inputs (list font-dejavu))
      (inputs (list xorg-server unzip))
      (propagated-inputs
       (list emacs-canvas-keys emacs-canvas-diagram
             emacs-transient emacs-websocket))
      (home-page "https://github.com/Daskeladden/canvas-browser")
      (synopsis "Browse web pages on an Emacs canvas through Chromium")
      (description
       "Canvas Browser renders web pages in Emacs 32 buffers using Chromium's
DevTools protocol and @code{canvas} images.  The @code{canvas-browser} command
provides keyboard navigation, link hints, editable fields, and text extraction.
Install @command{chromium} or @command{google-chrome} separately, or customize
@code{canvas-browser-chromium}, a list of executable names or paths.
@command{Xvfb} and @command{unzip} are included; the browser uses its own profile.")
      (license gpl3+))))
