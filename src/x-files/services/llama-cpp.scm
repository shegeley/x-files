(define-module (x-files services llama-cpp)

  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module ((gnu packages machine-learning) #:select (llama-cpp))
  #:use-module (gnu services shepherd)

  #:use-module (guix gexp)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy))

(define %gpt-oss-20b-gguf
  (let [(commit "c3303d94926e0e2262aacdd0fac4b18e1a29468e")]
    (package
      (name "gpt-oss-20b-gguf")
      (version "0.0")
      (build-system copy-build-system)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://huggingface.co/unsloth/gpt-oss-20b-GGUF")
                       (commit commit)))
                (file-name (string-append "gtp-oss-20b-gguf"))
                (sha256 (base32 "0ankwgkc7avswq06lippqj5xbllk9g1x3qs0d5bbf2c1qh63wk7n"))))
      (home-page "")
      (synopsis "")
      (description "")
      (license #f))))

(define* (llama-cpp-default-config
          #:key
          (model (string-append
                  "/var/lib/llama-cpp"
                  "/gpt-oss-20b-GGUF"
                  "/gpt-oss-20b-F16.gguf"))
          (port "9090"))
  `((command .
             ,#~(make-forkexec-constructor
                 (list
                  #$(file-append llama-cpp "/bin/llama-server")
                  "-m" #$model
                  "-c" "16384"
                  "-np" "4"
                  "--port" port)
                 #:log-file "/var/log/llama-cpp.log"
                 #:user "llama-cpp" #:group "llama-cpp"))))

(define %llama-cpp-accounts
  (list (user-account
          (name "llama-cpp")
          (group "llama-cpp")
          (system? #t)
          (comment "lLama-cpp daemon user")
          (home-directory "/var/lib/llama-cpp")
          (shell (file-append shadow "/sbin/nologin")))
        (user-group
          (name "llama-cpp")
          (system? #t))))

(define (llama-cpp-shepherd-service config)
  (list
   (shepherd-service
     (provision '(llama-cpp))
     (requirement '(user-processes networking))
     (start #~(make-forkexec-constructor
               ;; (assoc-ref config 'command)
               (list
                #$(file-append llama-cpp "/bin/llama-server")

                "-m" (string-append
                      "/var/lib/llama-cpp"
                      "/models"
                      "/gpt-oss-20b-GGUF"
                      "/gpt-oss-20b-F16.gguf")

                "-c" "16384"
                "-np" "4"
                "--port" "9090")
               #:log-file "/var/log/llama-cpp.log"
               #:user "llama-cpp" #:group "llama-cpp"))
     (auto-start? #t)
     (stop #~(make-kill-destructor))
     (documentation "Run llama-cpp server"))))

(define-public llama-cpp-service-type
  (service-type
    (name 'llama-cpp)
    (extensions
     (list (service-extension account-service-type
                              (const %llama-cpp-accounts))
           (service-extension shepherd-root-service-type
                              llama-cpp-shepherd-service)))
    (default-value llama-cpp-default-config)
    (description "Llama cpp")))
