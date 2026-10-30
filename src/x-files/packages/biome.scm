(define-module (x-files packages biome)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (%current-system %current-target-system))
  #:use-module ((guix build-system copy) #:select (copy-build-system))
  #:use-module ((guix licenses) #:prefix license:)
  #:export (biome))

(define version "2.6.4")

(define system->release
  ;; musl builds are static: no patchelf.  npm `dist.integrity' (sha512)
  ;; verified, then hashed with `guix hash'.
  '(("x86_64-linux"  . (("arch" . "x64")
                        ("hash" . "10k6xi6r06ws8nah8sjp16zjcm7pzi00g5r6lf3zpij4lqsllwcc")))
    ("aarch64-linux" . (("arch" . "arm64")
                        ("hash" . "1pmdk67pg5f6026h3b34h30s5zgpqklbx8s4cv5d9f5c1qvap7py")))))

(define (release-origin system)
  "The upstream npm tarball for @var{system}, falling back to x86_64."
  (let* ((release (or (assoc-ref system->release system)
                      (assoc-ref system->release "x86_64-linux")))
         (name    (string-append "cli-linux-" (assoc-ref release "arch") "-musl")))
    (origin
      (method url-fetch)
      (uri (string-append "https://registry.npmjs.org/@biomejs/" name "/-/"
                          name "-" version ".tgz"))
      (sha256 (base32 (assoc-ref release "hash"))))))

(define biome
  (package
    (name "biome")
    (version version)
    ;; Chosen in the (thunked) arguments: `source' is evaluated once at
    ;; module load, which would pin the host system's tarball.
    (source #f)
    (build-system copy-build-system)
    (arguments
     (list
      #:strip-binaries? #f
      #:install-plan #~'(("biome" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (invoke "tar" "xzf"
                      #$(release-origin (or (%current-target-system)
                                            (%current-system))))
              (chdir "package"))))))
    (supported-systems (map car system->release))
    (home-page "https://biomejs.dev/")
    (synopsis "Formatter, linter and language server for web languages")
    (description "Biome formats and lints JavaScript, TypeScript, JSX, JSON,
CSS and GraphQL.  @command{biome lsp-proxy} serves the same checks over the
Language Server Protocol.")
    (license (list license:expat license:asl2.0))))
