(define-module (x-files packages zeroclaw)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module (nonguix build-system binary)

  #:use-module ((gnu packages gcc) #:select (gcc))

  #:use-module ((guix licenses) #:prefix license:))

(define zeroclaw-version "0.1.7-beta.30")

(define target->zeroclaw-target
  ;; TODO: add others
  '(("x86_64-linux"  . "x86_64-unknown-linux-gnu")))

(define targets (map car target->zeroclaw-target))

(define target->zeroclaw-hash
  ;; TODO: add others
  '(("x86_64-linux"  . "1nzi72m1nrdb9dgwxc9zk1q3cg4fr34ik9fvi806wa88l545q11h")))

(define-public zeroclaw
  (let* [(target (or (%current-target-system) (%current-system)))
         (zeroclaw-target (assoc-ref target->zeroclaw-target target))
         (archive-filename (string-append "zeroclaw-" zeroclaw-target ".tar.gz"))
         (hash (assoc-ref target->zeroclaw-hash target))]
    (package
      (name "zeroclaw")
      (version (string-append "v" zeroclaw-version))
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/"
                      "zeroclaw-labs/zeroclaw/"
                      "releases/download/"
                      "v" version "/" archive-filename))
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments (list
                  #:validate-runpath? #f
                  #:patchelf-plan `'(("zeroclaw" ("gcc")))
                  #:install-plan `'(("./zeroclaw" "/bin/"))
                  #:phases #~(modify-phases %standard-phases
                               (add-after 'unpack 'chmod
                                 (lambda _ (chmod "./zeroclaw" #o755))))))
      (supported-systems targets)
      (inputs (list `(,gcc "lib")))
      (home-page "https://zeroclaw.org")
      (synopsis "ZeroClaw is the runtime operating system for agentic workflows —
infrastructure that abstracts models, tools, memory, and execution so
agents can be built once and run anywhere")
      (description "Features:
@itemize
@item 🏎️ Lean Runtime by Default: Common CLI and status workflows run
in a few-megabyte memory envelope on release builds.
@item 💰 Cost-Efficient Deployment: Designed for low-cost boards and
small cloud instances without heavyweight runtime dependencies.
@item ⚡ Fast Cold Starts: Single-binary Rust runtime keeps command
and daemon startup near-instant for daily operations.
@item 🌍 Portable Architecture: One binary-first workflow across ARM,
x86, and RISC-V with swappable providers/channels/tools.
@end")
      (license (list license:expat license:asl2.0)))))
