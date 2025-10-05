(define-module (x-files packages deno)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"  . "deno-x86_64-unknown-linux-gnu.zip")
    ("aarch64-linux" . "deno-aarch64-unkown-linux-gnu.zip")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "0rza5bgzv6mbm57qikgq5fml680j0h4363pfdl5by5lnphgxs6j2")
    ("aarch64-linux" . "1wdn33vc4lxz6kva0wdhj7qqdqmvfwira3hlx7rlmydfdfx2rbbq")))

(define-public deno
  (let* [(target    (or (%current-target-system) (%current-system)))
         (deno.bin  (assoc-ref target->bin-name target))
         (hash      (assoc-ref target->hash target))
         (version   "2.5.3")
         (uri       (string-append
                     "https://github.com/denoland/"
                     "deno/releases/download/"
                     "v" version
                     "/" deno.bin))]
    (package
      (name "deno")
      (version version)
      (source (origin
                (method url-fetch/zipbomb)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:validate-runpath? #f
        #:patchelf-plan `'(("deno" ("gcc")))
        #:install-plan `'(("./deno" "/bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _ (chmod "./deno" #o755))))))
      (inputs (list `(,gcc "lib")))
      (supported-systems targets)
      (home-page "https://deno.com/")
      (synopsis "A modern runtime for JavaScript and TypeScript")
      (description "Deno (/ˈdiːnoʊ/, pronounced dee-no) is a JavaScript, TypeScript, and WebAssembly runtime with secure defaults and a great developer experience. It's built on V8, Rust, and Tokio.")
      (license license:expat))))
