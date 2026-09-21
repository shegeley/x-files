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
  '(("x86_64-linux"  . "1z7f30vlr0wmlmmrj1hmhxqan4pyh5h6g7z3akhfhnjx7zhc5g20")
    ("aarch64-linux" . "0sx18hqfw5m898lvhd8vxlpl7s4s7kjwmrqljpdzhlyxz8m81w05")))

(define-public zellij
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->zellij-arch target))
         (hash   (assoc-ref target->zellij-hash target))]
    (package
      (name "zellij")
      (version "0.45.1")
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
