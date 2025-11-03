(define-module (x-files packages clojure)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix build-system copy) #:select (copy-build-system))
  #:use-module ((gnu packages readline) #:select (rlwrap))

  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix gexp) #:select (gexp)))

(define-public clojure-tools
  (package
    (name "clojure-tools")
    (version "1.12.1.1550") ;; 03.06.2025
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.clojure.org/install/clojure-tools-" version ".tar.gz"))
       (sha256 (base32 "0gqd9d137zd2c6jrkx8xxqb3vn51j9is922rhwsp0bn7g5b64v4h"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("deps.edn" "lib/clojure/")
          ("example-deps.edn" "lib/clojure/")
          ("tools.edn" "lib/clojure/")
          ("exec.jar" "lib/clojure/libexec/")
          (#$(string-append "clojure-tools-" version ".jar") "lib/clojure/libexec/")
          ("clojure" "bin/")
          ("clj" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "clojure"
                (("PREFIX") (string-append #$output "/lib/clojure")))
              (substitute* "clj"
                (("BINDIR") (string-append #$output "/bin"))
                (("rlwrap") (which "rlwrap"))))))))
    (inputs (list rlwrap))
    (home-page "https://clojure.org/releases/tools")
    (synopsis "CLI tools for the Clojure programming language")
    (description "The Clojure command line tools can be used to start a
Clojure repl, use Clojure and Java libraries, and start Clojure programs.")
    (license license:epl1.0)))
