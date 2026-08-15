(define-module (x-files packages guile-ares-rs-hot-reload)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages guile-xyz) #:select (guile-ares-rs))
  #:use-module ((x-files packages guile-fsnotify) #:select (guile-fsnotify))

  #:export (guile-ares-rs-hot-reload))

(define-public guile-ares-rs-hot-reload
  (package
   (name "guile-ares-rs-hot-reload")
   (version "0.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/shegeley/guile-ares-rs-hot-reload")
           (commit "ec32b043bb0a63fb794471f36a544a659e74e2fa")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "17ik3inmi0lrrdwpmbr4vaf9s9sa26w2l7n7blff0qwngrr6l4q1"))))
   (build-system guile-build-system)
   (arguments
    (list
     ;; The module's own name is (ares-extension nrepl hot-reload), so the
     ;; checkout's guile/ subdirectory (not the checkout root) is the
     ;; module root --- keeps tests.scm/tests/ out of the build.
     #:source-directory "guile"
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'build 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "guile"
                     "--no-auto-compile"
                     "-L" "guile"
                     "-L" "tests"
                     "-s" "tests.scm"))))))
   (propagated-inputs (list guile-ares-rs guile-fsnotify))
   (native-inputs (list guile-3.0-latest))
   (synopsis "guile-ares-rs nREPL extension for hot-reloading Guile modules and CSS")
   (description
    "Watches a directory tree of Guile @code{.scm} files and reloads
changed ones with @code{load}, and watches a CSS file to invoke a callback
on change --- both via Linux @code{inotify(7)}, no polling.  Exposes
@code{nrepl.hot-reload/start}, @code{/stop} and @code{/status} as
guile-ares-rs nREPL ops, parameterized entirely through the nREPL message,
so it carries no project-specific assumptions.")
   (home-page "https://codeberg.org/shegeley/guile-ares-rs-hot-reload")
   (license license:gpl3+)))
