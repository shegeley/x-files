(define-module (x-files packages vault)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module ((gnu packages compression) #:select (unzip))
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (nonguix licenses))

(define vault-version "2.0.4")

(define target->vault-target
  '(("x86_64-linux"  . "amd64")
    ("aarch64-linux" . "arm64")))

(define target->vault-hash
  '(("x86_64-linux"  . "11zk5nxzpvbmrhkc04c55jpa113r1x14j73hcgq9vwlfbzcffabl")
    ("aarch64-linux" . "10y696n9s4pnjxk5xn3k1f3byzq379wcvy2lrza0raacs3ynifw7")))

(define-public vault
  (let* [(target      (or (%current-target-system) (%current-system)))
         (vault-target (assoc-ref target->vault-target target))
         (hash         (assoc-ref target->vault-hash target))]
    (package
      (name "vault")
      (version vault-version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://releases.hashicorp.com/vault/" version
               "/vault_" version "_linux_" vault-target ".zip"))
         (sha256 (base32 hash))))
      (build-system copy-build-system)
      (native-inputs (list unzip))
      (arguments
       (list
        #:install-plan #~'(("vault" "bin/"))
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                (invoke "unzip" source)
                (chmod "vault" #o755))))))
      (supported-systems (map car target->vault-target))
      (home-page "https://www.vaultproject.io")
      (synopsis "Tool for secrets management, encryption, and privileged access management")
      (description
       "Vault secures, stores, and tightly controls access to tokens, passwords,
certificates, API keys, and other secrets.  It handles leasing, key revocation,
key rolling, and auditing.  It provides encryption as a service with integrated
secret management.")
      (license (nonfree "https://www.hashicorp.com/bsl")))))
