(define-module (x-files services llama-cpp)

  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module ((gnu packages machine-learning) #:select (llama-cpp))
  #:use-module ((gnu system accounts) #:select (user-account
                                                user-group
                                                subid-range))
  #:use-module ((gnu system shadow) #:select (account-service-type))

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (gnu services shepherd))

(define str string-append)

;; models as packages is kinda pointless because default guix timeout on quite execution of git-fetch/lfs is 3600 seconds. the weight much more
#;(define %gpt-oss-20b-gguf
   (let [(commit "c3303d94926e0e2262aacdd0fac4b18e1a29468e")]
     (package
       (name "gpt-oss-20b-gguf")
       (version "0.0")
       (build-system copy-build-system)
       (source (origin
                 (method git-fetch/lfs)
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
          (model (str "/data/models"
                      "/Mistral-7B-Instruct-v0.2-GGUF"
                      "/mistral-7b-instruct-v0.2.Q4_K_M.gguf"))
          (port "9090"))
  (let* [(cmd #~(list
                 #$(file-append llama-cpp "/bin/llama-server")
                 "-m" #$model
                 "-c" "16384"
                 "-np" "4"
                 "--port" #$port))]
    `((command . ,#~(make-forkexec-constructor
                     #$cmd
                     #:log-file "/var/log/llama-cpp.log"
                     #:user "llama-cpp"
                     #:group "llama-cpp")))))

(define %llama-cpp-accounts
  (list
   (user-account
     (name "llama-cpp")
     (group "llama-cpp")
     (system? #t)
     (comment "Lama-cpp daemon user")
     (home-directory "/var/lib/llama-cpp")
     (shell (file-append shadow "/sbin/nologin")))
   (user-group
     (name "llama-cpp")
     (system? #t))))

(define (llama-cpp-shepherd-service config)
  (list
   (shepherd-service
     (provision     '(llama-cpp))
     (requirement   '(user-processes networking))
     (start         (assoc-ref config 'command))
     (auto-start?   #t)
     (stop          #~(make-kill-destructor))
     (documentation "Run llama-cpp server"))))

(define-public llama-cpp-service-type
  (service-type
    (name 'llama-cpp)
    (extensions
     (list (service-extension account-service-type
                              (const %llama-cpp-accounts))
           (service-extension shepherd-root-service-type
                              llama-cpp-shepherd-service)))
    (default-value (llama-cpp-default-config))
    (description "Llama cpp")))
