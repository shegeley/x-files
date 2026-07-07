(define-module (x-files packages officecli)
  #:use-module ((gnu packages base) #:select (glibc))
  #:use-module ((gnu packages bash) #:select (bash-minimal))
  #:use-module ((gnu packages elf) #:select (patchelf))
  #:use-module (gnu packages gcc)
  #:use-module ((gnu packages icu4c) #:select (icu4c))
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

(define target->bin-name
  '(("x86_64-linux"  . "officecli-linux-x64")
    ("aarch64-linux" . "officecli-linux-arm64")))

(define targets (map car target->bin-name))

(define target->hash
  '(("x86_64-linux"  . "0na1ndfwcn2sf8fpmd259px4fyfiwawb664xwjr69imlhrzkai0y")
    ("aarch64-linux" . "0rbb3qrj0vcbxq1dfi7rshqkzdq6hjfm1sdcwrrg3r43x03r9nd2")))

(define-public officecli
  (let* [(target        (or (%current-target-system) (%current-system)))
         (officecli.bin (assoc-ref target->bin-name target))
         (hash          (assoc-ref target->hash target))
         (version       "1.0.129")
         (uri           (string-append
                         "https://github.com/iOfficeAI/"
                         "OfficeCLI/releases/download/"
                         "v" version
                         "/" officecli.bin))]
    (package
      (name "officecli")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (file-name "officecli")
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        ;; No #:patchelf-plan: OfficeCLI is a self-contained .NET single-file
        ;; app.  patchelf's --set-rpath rewrites the ELF and shifts the bundle
        ;; appended after it, corrupting it ("Arithmetic overflow while reading
        ;; bundle").  Instead patch only the interpreter (which preserves the
        ;; bundle) and inject the library path via a wrapper.
        #:validate-runpath? #f
        ;; Stripping also rewrites the ELF and corrupts the appended bundle.
        #:strip-binaries? #f
        ;; Install the real binary under a plain name in libexec: OfficeCLI
        ;; derives its root command name from the executable file name, and the
        ;; leading dot in wrap-program's ".officecli-real" makes .NET's
        ;; GetFileNameWithoutExtension return "" (empty command name -> crash).
        #:install-plan `'(("officecli" "/libexec/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _ (chmod "officecli" #o755)))
            (add-after 'chmod 'set-interpreter
              (lambda _
                (invoke "patchelf" "--set-interpreter"
                        (car (find-files
                              (string-append #$glibc "/lib")
                              "^ld-linux.*\\.so.*"))
                        "officecli")))
            (add-after 'install 'wrap
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((bin  (string-append #$output "/bin"))
                      (real (string-append #$output "/libexec/officecli"))
                      (libs (string-append
                             (assoc-ref inputs "glibc") "/lib:"
                             (assoc-ref inputs "gcc") "/lib:"
                             (assoc-ref inputs "icu4c") "/lib")))
                  (mkdir-p bin)
                  (call-with-output-file (string-append bin "/officecli")
                    (lambda (port)
                      (format port "#!~a
export LD_LIBRARY_PATH=\"~a${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}\"
exec \"~a\" \"$@\"~%"
                              (string-append (assoc-ref inputs "bash-minimal")
                                             "/bin/bash")
                              libs real)))
                  (chmod (string-append bin "/officecli") #o555)))))))
      (native-inputs (list patchelf))
      (inputs (list bash-minimal
                    glibc
                    `(,gcc "lib")
                    icu4c))
      (supported-systems targets)
      (home-page "https://officecli.ai/")
      (synopsis "AI-agent Office suite for Word, Excel, and PowerPoint files")
      (description "OfficeCLI is an Office suite purpose-built for AI agents to
read, edit, and automate Word, Excel, and PowerPoint files.  It ships as a
single self-contained binary and requires no Microsoft Office installation.")
      (license license:asl2.0))))
