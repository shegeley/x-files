(define-module (x-files packages 1s)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)           #:select (package origin base32))
  #:use-module ((guix download)           #:select (url-fetch))
  #:use-module (guix gexp)               ; required for #~ / #$ reader extensions
  #:use-module ((guix build-system copy)  #:select (copy-build-system))
  #:use-module ((gnu packages java)       #:select (openjdk)))

(define-public bsl-language-server
  (package
    (name "bsl-language-server")
    (version "0.29.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/1c-syntax/bsl-language-server/releases/download/v"
             version "/bsl-language-server-" version "-exec.jar"))
       (sha256
        (base32 "02ygkdvqy50dc11yx925b0w05rp4ihgsv20b4rg8alds73b9mynn"))))
    (build-system copy-build-system)
    (inputs (list openjdk))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-file #$source "bsl-language-server.jar")))
          (replace 'install
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((lib  (string-append #$output "/lib/bsl-language-server"))
                     (bin  (string-append #$output "/bin"))
                     (jar  (string-append lib "/bsl-language-server.jar"))
                     (java (string-append #$openjdk "/bin/java")))
                (mkdir-p lib)
                (mkdir-p bin)
                (copy-file "bsl-language-server.jar" jar)
                (call-with-output-file (string-append bin "/bsl-language-server")
                  (lambda (port)
                    (format port "#!/bin/sh\nexec ~a -jar ~a \"$@\"\n" java jar)))
                (chmod (string-append bin "/bsl-language-server") #o755)))))))
    (home-page "https://github.com/1c-syntax/bsl-language-server")
    (synopsis "LSP server for BSL (1C:Enterprise) and OneScript")
    (description
     "BSL Language Server is a Language Server Protocol (LSP) implementation
for BSL (1C:Enterprise scripting language) and OneScript.  It provides
code diagnostics, formatting, hover documentation, and other IDE features
to any LSP-compatible editor.")
    (license license:lgpl3+)))
