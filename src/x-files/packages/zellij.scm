(define-module (x-files packages zellij)
  #:use-module (ice-9 match)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (nonguix build-system binary)
  #:use-module (guix download))

(define target->zellij-arch
  '(("x86_64-linux"  . "x86_64")
    ("aarch64-linux" . "aarch64")))

(define target->zellij-hash
  '(("x86_64-linux"  . "0lfpr8768jr6z0wsf8n89zrcycw6fqbnaa9819n50zv2i1kk8z0g")
    ("aarch64-linux" . "1ys53g41cgwp2zcx7852l7bcz7bk9465jv0k7nbnckb4896m7rhm")))

(define-public zellij
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->zellij-arch target))
         (hash   (assoc-ref target->zellij-hash target))]
    (package
      (name "zellij")
      (version "0.44.3")
      (source
       (origin
         (method url-fetch/tarbomb)
         (uri (string-append
               "https://github.com/zellij-org/zellij/releases/download/v"
               version "/zellij-" arch
               "-unknown-linux-musl.tar.gz"))
         (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments `(#:install-plan `(("zellij" "/bin/"))))
      (inputs `())
      (supported-systems (map car target->zellij-arch))
      (synopsis "A terminal workspace with batteries included")
      (description "Zellij is a workspace aimed at developers, ops-oriented people and anyone who loves the terminal. Similar programs are sometimes called «Terminal Multiplexers».
                    It's designed around the philosophy that one must not sacrifice simplicity for power, taking pride in its great experience out of the box as well as the advanced features it places at its users' fingertips and geared toward beginner and power users alike - allowing deep customizability, personal automation through layouts, true multiplayer collaboration, unique UX features such as floating and stacked panes, and a plugin system allowing one to create plugins in any language that compiles to WebAssembly.")
      (home-page "https://zellij.dev")
      (license expat))))
