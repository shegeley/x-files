(define-module (x-files packages typst)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"        . "typst-x86_64-unknown-linux-musl.tar.xz")
    ("aarch64-linux"       . "typst-aarch64-unkown-linux-musl.tar.xz")
    ("riscv64-linux"       . "typst-riscv64gc-unknown-linux-musl.tar.xz")
    ("arm-linux-gnueabihf" . "typst-armv7-unknown-linux-musleabi.tar.xz ")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"        . "0gv7nmlzqrabg1fpz30y50hr95j4n790j6id8bf8brf2zzz4n8bx")
    ("aarch64-linux"       . "1m7f9iaa21czk8kgavyyiqiavva0z7ybzxp0kq0kkdkzwpk7wnsg")
    ("riscv64-linux"       . "1jz2xhxazp98g8z7dfr7w88ik2qsrkavb2xqd7csgz5wzkrkixqj")
    ("arm-linux-gnueabihf" . "1hpdfhcw1mf5z3cyyxh0d8j9vq9xlcjh7051ap3j6ragpzkgrj0b")))

(define-public typst
  (let* [(target    (or (%current-target-system) (%current-system)))
         (typst.bin (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "0.13.1")
         (uri       (string-append
                     "https://github.com/typst/"
                     "typst/releases/download/"
                     "v" version "/" typst.bin))]
    (package
      (name "typst")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:validate-runpath? #f
        #:install-plan `'(("./typst" "/bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _ (chmod "./typst" #o755))))))
      (supported-systems targets)
      (home-page "https://typst.app")
      (synopsis "A new markup-based typesetting system that is powerful and easy to learn")
      (description "Typst is a new markup-based typesetting system
that is designed to be as powerful as LaTeX while being much easier to
learn and use. Typst has:

@itemize
@item Built-in markup for the most common formatting tasks
@item Flexible functions for everything else
@item A tightly integrated scripting system
@item Math typesetting, bibliography management, and more
@item Fast compile times thanks to incremental compilation
@item Friendly error messages in case something goes wrong
@end itemize")
      (license license:asl2.0))))
