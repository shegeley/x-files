(define-module (x-files packages cups)
  #:use-module (x-files utils base)
  #:use-module (x-files utils list)
  #:use-module (x-files packages cups drivers samsung)

  #:use-module (gnu packages)
  #:use-module (gnu packages cups)

  #:use-module (guix packages)

  #:use-module (guix gexp)

  #:use-module (guix utils)

  #:export (cups))

(define-public cups
  #| NOTE: "cups" won't resolve to package in geiser
  The whole point of this module is to overrige base @code{cups} package build phases with proper ones that properly handles «filters». All the stages are identical to original, only one slash symbol added. see [1]
  See: https://issues.guix.gnu.org/57375 |#
  (package
    (inherit cups-minimal)
    (inputs
     (cons
      samsung-drivers
      (map specification->package
           (list "avahi"
                 "coreutils"
                 "cups-filters"
                 "gnutls"
                 "linux-pam"
                 "zlib"))))
    (arguments
     (substitute-keyword-arguments
         (strip-keyword-arguments
          '(#:tests?)
          (package-arguments cups-minimal))
       ((#:configure-flags flags #~'())
        #~(append #$flags
                  (list "--with-languages=all"))) ; no ‘=all’ means none(!)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'patch-tests
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((filters #$(this-package-input "cups-filters"))
                      (catpath (string-append
                                #$(this-package-input "coreutils") "/bin/"))
                      (testdir (string-append (getcwd) "/tmp/")))
                  (mkdir testdir)
                  (substitute* "test/run-stp-tests.sh"
                    ((" *BASE=/tmp/") (string-append "BASE=" testdir))

                    ;; Allow installation of filters from the output directory
                    ;; and from cups-filters.
                    (("for dir in /usr/libexec/cups/filter /usr/lib/cups/filter")
                     (string-append
                      "for dir in "
                      (assoc-ref outputs "out") "/lib/cups/filter "
                      filters "/lib/cups/filter"))

                    ;; Check for charsets in the default cups-filters output.
                    (("/usr/share/cups/charsets")
                     (string-append filters "/share/cups/charsets"))

                    ;; Install additional required filters.
                    (("instfilter texttopdf texttopdf pdf")
                     (string-append
                      "instfilter texttopdf texttopdf pdf;"
                      "instfilter imagetoraster imagetoraster raster;"
                      "instfilter gstoraster gstoraster raster;"
                      "instfilter urftopdf urftopdf pdf;"
                      "instfilter rastertopdf rastertopdf pdf;"
                      "instfilter pstopdf pstopdf pdf"))

                    ;; Specify the location of the lpstat binary.
                    (("description=\"`lpstat -l")
                     "description=\"`../systemv/lpstat -l")

                    ;; Patch the shebangs of embedded scripts.
                    (("#!/bin/sh") (string-append "#!" (which "sh")))

                    ;; Also link MIME definitions from cups-filters
                    ;; to enable the additional filters for the test suite.
                    (("ln -s \\$root/conf/mime\\.types")
                     (string-append
                      "ln -s " filters
                      "/share/cups/mime/cupsfilters.types $BASE/share/mime; "
                      "ln -s $root/conf/mime.types"))
                    (("ln -s \\$root/conf/mime\\.convs")
                     (string-append
                      "ln -s " filters
                      "/share/cups/mime/cupsfilters.convs $BASE/share/mime; "
                      "ln -s $root/conf/mime.convs")))

                  ;; Fix the search path for the "cat" command.
                  (substitute* "cups/testfile.c"
                    (("cupsFileFind\\(\"cat\", \"/bin\"")
                     (string-append "cupsFileFind(\"cat\", \"" catpath "\""))
                    (("cupsFileFind\\(\"cat\", \"/bin:/usr/bin\"")
                     (string-append "cupsFileFind(\"cat\", \"" catpath "\""))))))
            (add-after 'install 'install-cups-filters-symlinks
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out"))
                      (cups-filters #$(this-package-input "cups-filters"))
                      (samsung-drivers (this-package-input "samsung-filters")))
                  ;; Charsets.
                  (symlink
                   (string-append cups-filters "/share/cups/charsets")
                   (string-append out "/share/charsets"))

                  ;; MIME types, driver files, and PPDs.
                  (for-each
                   (lambda (f)
                     (symlink (string-append cups-filters f)
                              (string-append out f)))
                   '("/share/cups/mime/cupsfilters.types"
                     "/share/cups/mime/cupsfilters.convs"
                     "/share/cups/drv/cupsfilters.drv"))

                  (for-each
                   (lambda (f)
                     ;; NOTE: symlink = very dirty
                     (symlink (string-append samsung-drivers f)
                              (string-append out f)))
                   '("/share/ppd"))

                  ;; Filters.
                  (for-each
                   (lambda (f)
                     (symlink f
                              (string-append out "/lib/cups/filter/"
                                             ;; NOTE: [1]
                                             (basename f))))
                   (find-files (string-append cups-filters "/lib/cups/filter")))

                  ;; Backends.
                  (for-each
                   (lambda (f)
                     (symlink (string-append cups-filters f)
                              (string-append out "/lib/cups/backend/"
                                             (basename f))))
                   '("/lib/cups/backend/parallel"
                     "/lib/cups/backend/serial"))

                  ;; Banners.
                  (let ((banners "/share/cups/banners"))
                    (delete-file-recursively (string-append out banners))
                    (symlink (string-append cups-filters banners)
                             (string-append out banners)))

                  ;; Assorted data.
                  (let ((data "/share/cups/data"))
                    (delete-file-recursively (string-append out data))
                    (symlink (string-append cups-filters data)
                             (string-append out data))))))))))))
