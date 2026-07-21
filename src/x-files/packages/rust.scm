(define-module (x-files packages rust)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix build-system cargo) #:select (cargo-build-system
                                                    cargo-inputs))
  #:use-module ((x-files packages rust-crates) #:select (lookup-cargo-inputs)))

(define-public tramp-rpc-server
  (package
    (name "tramp-rpc-server")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ArthurHeymans/emacs-tramp-rpc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yiwa7hsc8anj1l6902ycwmcxjwkl4crrn7h84797m2dbrh524gm"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:cargo-install-paths ''("server")))
    (inputs (cargo-inputs 'tramp-rpc-server
                          #:module '(x-files packages rust-crates)))
    (home-page "https://github.com/ArthurHeymans/emacs-tramp-rpc")
    (synopsis "RPC server for TRAMP remote file access")
    (description
     "A lightweight Rust server for remote file operations.  Replaces TRAMP's
shell command parsing with fast JSON-RPC over SSH, speeding up directory
listings and other operations.")
    (license license:gpl3+)))
