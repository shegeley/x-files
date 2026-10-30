(define-module (x-files packages typescript)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (%current-system %current-target-system))
  #:use-module ((guix build-system copy) #:select (copy-build-system))
  #:use-module ((guix licenses) #:prefix license:)
  #:export (typescript-native))

(define version "7.0.2")

(define system->release
  ;; npm `dist.integrity' (sha512) verified, then hashed with `guix hash'.
  '(("x86_64-linux"  . (("arch" . "x64")
                        ("hash" . "1lrphqs3kw0gmkw5nh0ilq31bwllycp0dav7cf2k3s3pfgvddjky")))
    ("aarch64-linux" . (("arch" . "arm64")
                        ("hash" . "1snh3psdrdg3gwkkxpr36221563aknm4c4p7wv6ljxfxr4d96gf8")))))

(define (release-origin system)
  "The upstream tarball for @var{system}, falling back to x86_64."
  (let ((release (or (assoc-ref system->release system)
                     (assoc-ref system->release "x86_64-linux"))))
    (origin
      (method url-fetch)
      (uri (string-append "https://registry.npmjs.org/@typescript/"
                          "typescript-linux-" (assoc-ref release "arch") "/-/"
                          "typescript-linux-" (assoc-ref release "arch")
                          "-" version ".tgz"))
      (sha256 (base32 (assoc-ref release "hash"))))))

(define typescript-native
  (package
    (name "typescript-native")
    (version version)
    ;; Chosen in the (thunked) arguments: `source' is evaluated once at
    ;; module load, which would pin the host system's tarball.
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      ;; Statically linked Go binary: nothing to patch, nothing to strip.
      #:strip-binaries? #f
      ;; tsc looks up lib.*.d.ts next to its resolved executable.
      #:install-plan #~'(("lib/" "lib/typescript/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "tar" "xzf"
                      #$(release-origin (or (%current-target-system)
                                            (%current-system))))
              (chdir "package")))
          (add-after 'install 'link-tsc
            (lambda _
              (mkdir-p (string-append #$output "/bin"))
              (symlink "../lib/typescript/tsc"
                       (string-append #$output "/bin/tsc")))))))
    (supported-systems (map car system->release))
    (home-page "https://www.typescriptlang.org/")
    (synopsis "Native (Go) TypeScript compiler and language server")
    (description "TypeScript 7 is the native port of the TypeScript compiler.
Besides type-checking and emitting JavaScript, @command{tsc --lsp --stdio}
runs a language server, so editors need neither Node.js nor
@code{typescript-language-server}.")
    (license license:asl2.0)))
