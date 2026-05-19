(define-module (x-files packages qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix build-system qt) #:select (qt-build-system))
  #:use-module ((gnu packages pkg-config) #:select (pkg-config))
  #:use-module ((gnu packages qt) #:select (qtbase qt5compat))

  #:export (qbs))

(define-public qbs
  ;; TODO: remove when merged https://codeberg.org/guix/guix/pulls/8586
  (package
    (name "qbs")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://qt/qbs/"
                                  version "/qbs-src-" version ".tar.gz"))
              (sha256
               (base32
                "0700xffwjjnx9m8h11b9kvv37pfmha03aqvk7ydyf94y3cij8mr4"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f
      #:configure-flags
      #~(list "-DWITH_TESTS=OFF"
              "-DQBS_INSTALL_PREFIX=/")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-qt5compat-includes
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((qt5c (assoc-ref inputs "qt5compat")))
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-append
                         qt5c "/include/qt6" ":"
                         (or (getenv "CPLUS_INCLUDE_PATH") "")))))))))
    (inputs
     (list qtbase qt5compat))
    (native-inputs
     (list pkg-config))
    (home-page "https://qbs.io")
    (synopsis "Qt Build Suite, a build automation tool")
    (description
     "Qbs (Qt Build Suite) is a build automation tool designed to conveniently
manage the build process of software projects across multiple platforms.  Qbs
can be used for any software project, regardless of programming language,
toolkit, or libraries used.")
    (license license:lgpl3+)))
