(define-module (x-files packages emacs canvas)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix git-download)
                #:select (git-fetch git-reference git-version git-file-name))
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module (guix gexp)
  #:use-module ((gnu packages commencement) #:select (gcc-toolchain))
  #:use-module ((gnu packages emacs) #:select (emacs-next-pgtk))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-transient))
  #:use-module ((gnu packages fonts) #:select (font-dejavu))
  #:use-module ((gnu packages gnome) #:select (librsvg))
  #:use-module ((gnu packages gtk) #:select (cairo pango gdk-pixbuf))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config)))

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
for packages that display Emacs 32 canvas images.")
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
export for Emacs 32 canvas images.  Its native Cairo and Pango module supports
SVG icons and PNG, JPEG, and GIF pictures through librsvg and GdkPixbuf.")
      (license gpl3+))))
