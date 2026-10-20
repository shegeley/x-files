(define-module (x-files packages coder)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

;; https://github.com/coder/coder/releases/download/v2.22.0/coder_2.22.0_linux_arm64.tar.gz
;; https://github.com/coder/coder/releases/download/v2.22.0/coder_2.22.0_linux_armv7.tar.gz
;; https://github.com/coder/coder/releases/download/v2.22.0/coder_2.22.0_linux_amd64.tar.gz

(define coder-license
  ((@@ (guix licenses) license)
   "Coder License"
   "https://github.com/coder/coder/blob/main/LICENSE.enterprise"
   ""))

(define target->arch-name
  '(("x86_64-linux"  . "amd64")
    ("aarch64-linux" . "arm64")
    ("armhf-linux"   . "armv7")))

(define targets (map car target->arch-name))

(define target->hash
  '(("x86_64-linux"  . "15cmgvdnzd6r8i98ls3bskwz5hyzaqk1jyp7lgqhsyr0m2s317l9")
    ("aarch64-linux" . "08qzn5sq73nj83w3i6krgv8gxw8vd15n7nwnz9jc0hx3vw9wh2pa")
    ("armhf-linux"   . "1z046j0dhz60yv0ksgnaxlasjpcmszwk3s7gslsafvyr6z4r78x0")))

(define-public coder
  (let* [(target  (or (%current-target-system) (%current-system)))
         (arch    (assoc-ref target->arch-name target))
         (hash    (assoc-ref target->hash target))
         (version "2.37.5")
         (uri     (string-append
                   "https://github.com/coder/"
                   "coder/releases/download/"
                   "v" version "/"
                   "coder_" version "_linux_" arch ".tar.gz"))]
    (package
      (name "coder")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256 (base32 hash))))
      (build-system binary-build-system)
      (arguments
       (list
        #:validate-runpath? #f
        #:install-plan `'(("./coder" "/bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chmod
              (lambda _ (chmod "./coder" #o755))))))
      (supported-systems targets)
      (home-page "https://coder.com")
      (synopsis "Secure environments for developers and their agents")
      (description "Bring developers and coding agents together in
self-hosted environments on your infrastructure. Open source and
deployable anywhere, Coder helps enterprises adopt AI without
compromising security or performance.")
      (license (list license:agpl3 coder-license)))))

coder
