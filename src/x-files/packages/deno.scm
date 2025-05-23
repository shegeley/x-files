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

(define-public deno
  (let* [(target (or (%current-target-system) (%current-system)))
         (deno.bin-name
          (string-append
           "deno-"
           (match target
             ("x86_64-linux"  "x86_64")
             ("aarch64-linux" "aarch64"))
           "-unknown-linux-gnu.zip"))
         (hash
          (match target
            ("x86_64-linux"  "1mb6vn1srwqlp4dlyky6lp433kfgb4srrrq76ngjzq6h6fxghzmv")
            ("aarch64-linux" "0x1062r0m5xg950l0zgpibkmz16fbppgv7s6ixgc2dkjjikg6m8p")))]
    (package
      (name "deno")
      (version "2.2.12")
      (source (origin
                (method url-fetch/zipbomb)
                (uri (string-append "https://github.com/denoland/"
                                    "deno/releases/download/"
                                    "v" version
                                    "/" deno.bin-name))
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
      (supported-systems '("x86_64-linux" "aarch64-linux"))
      (home-page "https://deno.com/")
      (synopsis "A modern runtime for JavaScript and TypeScript")
      (description "Deno (/ˈdiːnoʊ/, pronounced dee-no) is a JavaScript, TypeScript, and WebAssembly runtime with secure defaults and a great developer experience. It's built on V8, Rust, and Tokio.")
      (license license:expat))))
