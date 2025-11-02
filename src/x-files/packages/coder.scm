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
    ("armv7-linux"   . "armv7")))

(define targets (map car target->arch-name))

(define target->hash
  '(("x86_64-linux"  . "05zb82d3gg54ra2ih04b4dgj145yzy7d3s1w2q5x6kkp5iy7g98a")
    ("aarch64-linux" . "1jgmabj9dz8p7cj84j04gbycvrzcxc9mjahl1nfz9h5sf8y4pbwa")
    ("armv7-linux"   . "1yjxnbzz56nc7cg07zzv22lysy5w76fnkqsmwkhr3n8rlrp94yj1")))

(define-public coder
  (let* [(target  (or (%current-target-system) (%current-system)))
         (arch    (assoc-ref target->arch-name target))
         (hash    (assoc-ref target->hash target))
         (version "2.26.3")
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
