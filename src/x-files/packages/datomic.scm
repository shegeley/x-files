(define-module (x-files packages datomic)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public datomic
  ;; stolen from https://codeberg.org/simendsjo/dotfiles.git
  (package
    (name "datomic")
    (version "1.0.7180")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://datomic-pro-downloads.s3.amazonaws.com/"
             version "/datomic-pro-" version ".zip"))
       (sha256
        (base32 "09rc1hsdia59p48vjbwsis7ggny2mvzr97k0pmsnlwvi84xaaq2d"))))
    (build-system copy-build-system)
    (inputs (list unzip openjdk))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "unzip" #$source)
              (chdir (string-append "datomic-pro-" #$version "/"))))
          (add-after 'unpack 'patch-locations
             (lambda _
               (let ((java-bin (string-append #$openjdk "/bin/java")))
                 ;; Should we rather wrap the scripts to set this variable?
                 (substitute* "bin/logback.xml"
                   (("\\$\\{DATOMIC_LOG_DIR:-log\\}") "${XDG_STATE_HOME}/log/datomic"))
                 (substitute* "bin/console"
                   (("/usr/bin/env java") java-bin))
                 (substitute* "bin/rest"
                   (("/usr/bin/env java") java-bin))
                 (substitute* "bin/repl-jline"
                   (("/usr/bin/env java") java-bin))
                 (substitute* "bin/run"
                   (("/usr/bin/env java") java-bin))
                 (substitute* "bin/shell"
                   (("java") java-bin))
                 (substitute* "bin/transactor"
                   (("exec java") (string-append "exec " java-bin)))))))))
    (home-page "https://datomic.com")
    (synopsis "The Datomic database")
    (description "The fully transactional, cloud-ready, distributed database.")
    (license license:asl2.0)))
