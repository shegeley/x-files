(define-module (x-files packages datomic)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define (make-datomic version hash)
  ;; stolen from https://codeberg.org/simendsjo/dotfiles.git
  (package
    (name "datomic")
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://datomic-pro-downloads.s3.amazonaws.com/"
             version "/datomic-pro-" version ".zip"))
       (sha256
        (base32 hash))))
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
                   (("/usr/bin/env java") java-bin)
                   (("while \\[ \\$# -gt 0 \\]")
                    "java_opts=()\nclojure_args=()\n\nwhile [ $# -gt 0 ]")
                   (("java_opts=\"\\$\\{java_opts\\} \\$1\"")
                    "java_opts+=(\"$1\")")
                   (("clojure_args=\"\\$\\{clojure_args\\} \\$1\"")
                    "clojure_args+=(\"$1\")")
                   (("\\$\\{java_opts\\}") "\"${java_opts[@]}\"")
                   (("\\$\\{clojure_args\\}") "\"${clojure_args[@]}\""))
                 (substitute* "bin/datomic"
                   (("\\$@") "\"$@\""))
                 (substitute* "bin/shell"
                   (("java") java-bin))
                 (substitute* "bin/transactor"
                   (("exec java") (string-append "exec " java-bin)))))))))
    (home-page "https://datomic.com")
    (synopsis "The Datomic database")
    (description "The fully transactional, cloud-ready, distributed database.")
    (license license:asl2.0)))

(define-public datomic
  (make-datomic "1.0.7745" "1p6jgfg724haw02z5fd61j45m71nqpkabxpfb9dbivsgy32npb30"))
