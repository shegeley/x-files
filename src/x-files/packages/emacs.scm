(define-module (x-files packages emacs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((gnu packages emacs-xyz) #:select (emacs-gptel
                                                   emacs-org-ql))
  #:use-module (gnu packages))

(define-public emacs-prisma-ts-mode
  (let [(url "https://github.com/nverno/prisma-ts-mode")
        (commit "c63117764dc9e177aea7ddbef23c47feba1523d8")
        (version "1.0.0")
        (hash "19mhcr7v11shhzf1p5b6wbb9b8hx1q0wpzwrxhgypmzvx9837mgz")]
    (package
      (name "emacs-prisma-ts-mode")
      (version (git-version version "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url url)
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (home-page url)
      (synopsis "Major mode for Prisma using tree-sitter")
      (description "Tree-sitter based major mode for Prisma schema files.
Provides support for indentation, font-locking, imenu, and structural navigation.")
      (license license:gpl3+))))

(define-public emacs-gptel-got
  (let [(url "https://codeberg.org/bajsicki/gptel-got")
        (commit "f25c56c4b3fae3bf435e3a82723438b49ac6878b")
        (version "0.0.2")
        (hash "1n0gpzi1liz38xxy2l3cp2jwcw5j2qf1vfz5i29pvhvffqxsz4kh")]
    (package
      (name "emacs-gptel-got")
      (version (git-version version "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url url)
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f)) ;; no tests
      (propagated-inputs (list emacs-gptel emacs-org-ql))
      (home-page url)
      (synopsis "LLM Tools for org-mode interaction")
      (description "This is a package which expands the functionality of gptel for interacting with org-mode")
      (license license:gpl3+))))
