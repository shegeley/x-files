(define-module (x-files packages guile-fsnotify)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages guile)

  #:export (guile-fsnotify))

(define-public guile-fsnotify
  (package
   (name "guile-fsnotify")
   (version "3.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/shegeley/guile-fsnotify")
           (commit "8ecd9c5dc465900068694f0e6124ca3149155804")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0dysqjxd7639qf0cj4s8p0b8m0aash5sfdfn780aav0r53hcsf3p"))))
   (build-system guile-build-system)
   (arguments
    (list #:source-directory "linux"))
   (native-inputs (list guile-3.0-latest))
   (synopsis "Linux inotify and fanotify bindings for GNU Guile")
   (description
    "Guile-fsnotify provides pure Scheme FFI bindings to the Linux
@code{inotify(7)} and @code{fanotify(7)} filesystem event notification
mechanisms for GNU Guile 3.0.  No C extension is required---it uses Guile's
@code{(system foreign)} module to call libc directly.  The modules are
@code{(linux inotify)} and @code{(linux fanotify)}.")
   (home-page "https://codeberg.org/shegeley/guile-fsnotify")
   (license license:gpl3+)))
