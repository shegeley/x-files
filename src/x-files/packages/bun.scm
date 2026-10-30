(define-module (x-files packages bun)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch/zipbomb))
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (%current-system %current-target-system))
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:use-module ((guix licenses) #:prefix license:)
  #:export (bun))

(define version "1.4.4")

(define system->release
  ;; Upstream SHASUMS256.txt, converted to nix-base32.
  '(("x86_64-linux"  . (("arch" . "x64")
                        ("hash" . "0p1rrgy55nry189wdnrlfl3xib8pag2iavcs1yg1b00b5gxwp35z")))
    ("aarch64-linux" . (("arch" . "aarch64")
                        ("hash" . "03l5qzw8dgwbdrvnsgb79x2xza9qa21fmfq23rb5hvhxylvbp9ms")))))

(define (release-origin system)
  "The upstream zip for @var{system} (unpacked), falling back to x86_64."
  (let ((release (or (assoc-ref system->release system)
                     (assoc-ref system->release "x86_64-linux"))))
    (origin
      (method url-fetch/zipbomb)
      (uri (string-append "https://github.com/oven-sh/bun/releases/download/"
                          "bun-v" version "/bun-linux-" (assoc-ref release "arch")
                          ".zip"))
      (sha256 (base32 (assoc-ref release "hash"))))))

(define bun
  (package
    (name "bun")
    (version version)
    ;; Chosen in the (thunked) arguments: `source' is evaluated once at
    ;; module load, which would pin the host system's zip.
    (source #f)
    (build-system binary-build-system)
    (arguments
     (list
      ;; `bun build --compile' copies this executable verbatim; keep it intact.
      #:strip-binaries? #f
      #:patchelf-plan #~'(("bun" ("libc")))
      #:install-plan #~'(("bun" "bin/"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda _
              (copy-recursively
               #$(release-origin (or (%current-target-system)
                                     (%current-system)))
               ".")
              (chdir (car (find-files "." "^bun-linux-" #:directories? #t)))
              (chmod "bun" #o755)))
          (add-after 'install 'install-bunx
            ;; bun dispatches on argv[0]; upstream ships bunx as this symlink.
            (lambda _
              (symlink "bun" (string-append #$output "/bin/bunx")))))))
    (supported-systems (map car system->release))
    (home-page "https://bun.com/")
    (synopsis "JavaScript runtime, bundler, test runner and package manager")
    (description "Bun is an all-in-one toolkit for JavaScript and TypeScript
apps.  It ships as a single executable: a Node.js-compatible runtime built on
JavaScriptCore, plus @command{bun install}, @command{bun test},
@command{bun build} and @command{bunx}.")
    (license license:expat)))
