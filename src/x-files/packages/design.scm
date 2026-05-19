(define-module (x-files packages design)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix build-system gnu) #:select (gnu-build-system))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages qt) #:select (qtbase qtsvg qttools qtwayland))
  #:use-module ((gnu packages xml) #:select (xerces-c))
  #:use-module ((x-files packages qt) #:select (qbs))

  #:export (valentina))

(define-public valentina
  ;; TODO: remove when deployed https://codeberg.org/guix/guix/pulls/8587
  (package
    (name "valentina")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://gitlab.com/smart-pattern/valentina/-/archive/v"
             version "/valentina-v" version ".tar.gz"))
       (sha256
        (base32 "1jazq187ib5dn10jzcx2vl3f938833i2pdgrgkc1m1zgsa74jy9f"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'remove-geofencing
            (lambda _
              ;; Remove country-based restriction that crashes the
              ;; application for users in certain countries.
              (substitute* (find-files "src/app" "\\.(cpp|h)$")
                (("if \\(country == \"ru\".*\\)")
                 "if (false)")
                (("qFatal\\(\"country not detected\"\\);") ""))))
          (add-before 'build 'set-home
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "HOME" (getcwd))
              ;; Ensure qtsvg headers are findable globally.
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-append
                       (assoc-ref inputs "qtsvg") "/include/qt6:"
                       (or (getenv "CPLUS_INCLUDE_PATH") "")))))
          (add-before 'build 'setup-qbs
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((qt (assoc-ref inputs "qtbase"))
                     (qtsvg (assoc-ref inputs "qtsvg"))
                     (gcc (assoc-ref inputs "gcc"))
                     (combined (string-append (getcwd) "/../qt-combined"))
                     (wrapper (string-append combined "/bin/qmake6")))
                ;; Create a combined Qt prefix so QBS can find
                ;; modules from all Qt packages in one place.
                (mkdir-p (string-append combined "/lib/qt6/mkspecs/modules"))
                (mkdir-p (string-append combined "/bin"))
                (copy-recursively
                 (string-append qt "/lib/qt6/mkspecs")
                 (string-append combined "/lib/qt6/mkspecs"))
                ;; Copy qtsvg .pri files with absolute paths replacing
                ;; qmake variables that would resolve to qtbase.
                (for-each
                 (lambda (f)
                   (let ((dest (string-append combined
                                              "/lib/qt6/mkspecs/modules/"
                                              (basename f))))
                     (unless (file-exists? dest)
                       (copy-file f dest)
                       (substitute* dest
                         (("\\$\\$QT_MODULE_LIB_BASE")
                          (string-append qtsvg "/lib"))
                         (("\\$\\$QT_MODULE_INCLUDE_BASE")
                          (string-append qtsvg "/include/qt6"))
                         (("\\$\\$QT_MODULE_BIN_BASE")
                          (string-append qtsvg "/bin"))))))
                 (find-files (string-append qtsvg "/lib/qt6/mkspecs/modules")
                             "\\.pri$"))
                ;; Create qt.conf that keeps Prefix at qtbase but
                ;; redirects ArchData/HostData to the combined dir.
                (with-output-to-file (string-append combined "/bin/qt.conf")
                  (lambda ()
                    (format #t "\
[Paths]
Prefix=~a
ArchData=~a/lib/qt6
HostData=~a/lib/qt6
Headers=~a/include/qt6
Libraries=~a/lib
Plugins=~a/lib/qt6/plugins
LibraryExecutables=~a/lib/qt6/libexec
HostLibraryExecutables=~a/lib/qt6/libexec
Binaries=~a/bin
HostBinaries=~a/bin
" qt combined combined qt qt qt qt qt combined combined)))
                ;; Create a qmake wrapper that uses our qt.conf.
                (with-output-to-file wrapper
                  (lambda ()
                    (format #t "#!~a~%exec ~a/bin/qmake6 -qtconf ~a/bin/qt.conf \"$@\"~%"
                            (which "sh") qt combined)))
                (chmod wrapper #o755)
                ;; Symlink qmake -> qmake6 wrapper.
                (symlink wrapper
                        (string-append combined "/bin/qmake"))
                ;; Symlink qttools binaries (lrelease, lupdate, etc.)
                (let ((qttools (assoc-ref inputs "qttools")))
                  (for-each
                   (lambda (f)
                     (let ((dest (string-append combined "/bin/"
                                                (basename f))))
                       (unless (file-exists? dest)
                         (symlink f dest))))
                   (find-files (string-append qttools "/bin"))))
                ;; Configure QBS toolchain and Qt profile.
                (invoke "qbs" "setup-toolchains"
                        (string-append gcc "/bin/g++") "gcc")
                (invoke "qbs" "setup-qt" wrapper "qt6")
                (invoke "qbs" "config" "profiles.qt6.baseProfile" "gcc")
                (invoke "qbs" "config" "defaultProfile" "qt6")
                (invoke "qbs" "config" "preferences.qbsSearchPaths"
                        (string-append (getcwd) "/qbs")))))
          (add-after 'setup-qbs 'disable-werror
            (lambda _
              ;; Disable -Werror to avoid build failure from
              ;; deprecated Qt API warnings with newer Qt.
              (substitute* "qbs/modules/buildconfig/buildconfig.qbs"
                (("\"(-Werror)\"") "")
                (("treatWarningsAsErrors: true")
                 "treatWarningsAsErrors: false"))))
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (invoke "qbs" "build" "-f" "valentina.qbs" "-d" "../build"
                      "--jobs" (if parallel-build?
                                   (number->string (parallel-job-count))
                                   "1")
                      "config:release" "project.enableConan:false"
                      (string-append
                       "moduleProviders.qbspkgconfig.extraPaths:"
                       (string-join
                        (string-split (getenv "PKG_CONFIG_PATH") #\:)
                        ",")))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "qbs" "install" "--no-build" "-d" "../build"
                      "--install-root" (assoc-ref outputs "out")
                      "config:release"))))))
    (native-inputs (list qbs qttools pkg-config))
    (inputs (list qtbase qtsvg qtwayland xerces-c))
    (home-page "https://smart-pattern.com.ua/valentina/")
    (synopsis "Pattern making program for sewing")
    (description
     "Valentina is a cross-platform pattern making program which allows designers
to create and model patterns of clothing.  Valentina uses standard sizing tables
or individual measurements to create patterns.  It includes tools for drafting,
grading, and laying out patterns.")
    (license license:gpl3+)))
