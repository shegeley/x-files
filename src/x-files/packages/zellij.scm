(define-module (x-files packages zellij)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download))

(define-public zellij
  (let ((arch-match (lambda (arch)
                      (match arch
                        ((or "x86_64-linux") "x86_64")
                        ((or "aarch64-linux") "aarch64")))))
    (package
      (name "zellij")
      (version "0.39.2")
      (source
       (origin
         (method url-fetch/tarbomb)
         (uri (string-append
               "https://github.com/zellij-org/zellij/releases/download/v"
               version "/zellij-"
               (arch-match
                (or (%current-target-system) (%current-system)))
               "-unknown-linux-musl.tar.gz"))
         (sha256 (base32 "10aamb7vy3rccgqgnpwmyspn1yhjz1sy8lr77f54saq4hwldl0wj"))))
      (build-system binary-build-system)
      (arguments `(#:install-plan `(("zellij" "/bin/"))))
      (inputs `())
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      (synopsis "A terminal workspace with batteries included")
      (description "Zellij is a workspace aimed at developers, ops-oriented people and anyone who loves the terminal. Similar programs are sometimes called «Terminal Multiplexers».
                    It's designed around the philosophy that one must not sacrifice simplicity for power, taking pride in its great experience out of the box as well as the advanced features it places at its users' fingertips and geared toward beginner and power users alike - allowing deep customizability, personal automation through layouts, true multiplayer collaboration, unique UX features such as floating and stacked panes, and a plugin system allowing one to create plugins in any language that compiles to WebAssembly.")
      (home-page "https://zellij.dev")
      (license expat))))
