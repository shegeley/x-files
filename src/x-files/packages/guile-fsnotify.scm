(define-module (x-files packages guile-fsnotify)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages guile-xyz) #:select (guile-ares-rs))

  #:export (guile-fsnotify))

(define-public guile-fsnotify
  (package
   (name "guile-fsnotify")
   (version
    (git-version "3.0.0" "0"
                 "2c203ec6154cbb6ae9b3ac7bdc34189e5570453f"))
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/shegeley/guile-fsnotify")
           (commit "2c203ec6154cbb6ae9b3ac7bdc34189e5570453f")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1wi6i40nb1mhljf73ghyvmgask6dcjiyxxl5cdn4c3k0q6qgbb29"))))
   (build-system guile-build-system)
   (arguments
    ;; Upstream moved linux/ under guile/ (f96dd6b) so guile/ is the module
    ;; root and tests.scm/tests/ stay out of the build.
    (list #:source-directory "guile"
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'update-test-runner-name
                (lambda _
                  (substitute* "tests.scm"
                    (("test-runner\\*") "current-test-runner"))))
              (add-after 'build 'check
                (lambda* (#:key (tests? #t) #:allow-other-keys)
                  (when tests?
                    (invoke "guile" "--no-auto-compile"
                            "-L" "guile" "-L" "tests" "-s" "tests.scm")))))))
   (native-inputs (list guile-3.0-latest guile-ares-rs))
   (synopsis "Linux inotify and fanotify bindings for GNU Guile")
   (description
    "Guile-fsnotify provides pure Scheme FFI bindings to the Linux
@code{inotify(7)} and @code{fanotify(7)} filesystem event notification
mechanisms for GNU Guile 3.0.  No C extension is required---it uses Guile's
@code{(system foreign)} module to call libc directly.  The modules are
@code{(linux inotify)} and @code{(linux fanotify)}.")
   (home-page "https://codeberg.org/shegeley/guile-fsnotify")
   (license license:gpl3+)))
