(define-module (x-files services llama-cpp)

  #:use-module ((gnu packages admin) #:select (shadow))
  #:use-module ((gnu packages machine-learning) #:select (llama-cpp))
  #:use-module ((gnu system accounts) #:select (user-account user-group))
  #:use-module ((gnu system shadow) #:select (account-service-type))
  #:use-module ((gnu services) #:select (service-type service-extension))
  #:use-module ((gnu services shepherd) #:select (shepherd-root-service-type
                                                   shepherd-service))
  #:use-module (guix gexp)
  #:use-module ((guix records) #:select (define-record-type*))
  #:use-module ((guix packages) #:select (package origin))
  #:use-module ((guix git-download) #:select (git-reference git-fetch/lfs))
  #:use-module ((guix build-system copy) #:select (copy-build-system))

  #:export (llama-cpp-model
            llama-cpp-configuration
            llama-cpp-service-type))

;; Per-package timeout override: the daemon normally kills a build after 3600s
;; of silence (max-silent-time) or 86400s total (timeout).  Large LFS fetches
;; (multi-GB model weights) easily exceed the silence limit.  Setting these in
;; package `properties` overrides the global daemon defaults for that specific
;; derivation only — see gnu/packages/linux.scm:%linux-libre-timeout-properties
;; for prior art and nix/libstore/globals.cc for the C-level defaults.
(define %gpt-oss-20b-gguf
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
                (file-name (string-append "gpt-oss-20b-gguf"))
                (sha256 (base32 "0ankwgkc7avswq06lippqj5xbllk9g1x3qs0d5bbf2c1qh63wk7n"))))
      (properties `((timeout         . ,(* 24 3600))  ;; 24h — large LFS fetch
                    (max-silent-time . ,(* 12 3600)))) ;; 12h — network stalls
      (home-page "https://huggingface.co/unsloth/gpt-oss-20b-GGUF")
      (synopsis "GPT OSS 20B model weights in GGUF format")
      (description "Quantized GGUF weights for the GPT OSS 20B model.")
      (license #f))))

(define-record-type* <llama-cpp-model>
  llama-cpp-model make-llama-cpp-model
  llama-cpp-model?
  (name         llama-cpp-model-name)           ;; string, used in shepherd provision and log file name
  (model-path   llama-cpp-model-path)           ;; path to .gguf file
  (port         llama-cpp-model-port            ;; port string
                (default "9090"))
  (context-size llama-cpp-model-context-size    ;; context window size string
                (default "16384"))
  (parallel     llama-cpp-model-parallel        ;; number of parallel slots string
                (default "4")))

(define-record-type* <llama-cpp-configuration>
  llama-cpp-configuration make-llama-cpp-configuration
  llama-cpp-configuration?
  (models llama-cpp-configuration-models))      ;; list of <llama-cpp-model>

(define %llama-cpp-accounts
  (list
   (user-account
     (name "llama-cpp")
     (group "llama-cpp")
     (system? #t)
     (comment "Llama-cpp daemon user")
     (home-directory "/var/lib/llama-cpp")
     (shell (file-append shadow "/sbin/nologin")))
   (user-group
     (name "llama-cpp")
     (system? #t))))

(define (llama-cpp-model->shepherd-service model)
  (let* [(name         (llama-cpp-model-name model))
         (model-path   (llama-cpp-model-path model))
         (port         (llama-cpp-model-port model))
         (context-size (llama-cpp-model-context-size model))
         (parallel     (llama-cpp-model-parallel model))
         (log-file     (string-append "/var/log/llama-cpp-" name ".log"))
         (provision    (list (string->symbol (string-append "llama-cpp-" name))))]
    (shepherd-service
      (provision     provision)
      (requirement   '(user-processes networking))
      (start         #~(make-forkexec-constructor
                        (list #$(file-append llama-cpp "/bin/llama-server")
                              "-m" #$model-path
                              "-c" #$context-size
                              "-np" #$parallel
                              "--port" #$port)
                        #:log-file #$log-file
                        #:user "llama-cpp"
                        #:group "llama-cpp"))
      (auto-start?   #t)
      (stop          #~(make-kill-destructor))
      (documentation (string-append "Run llama-cpp server for model: " name)))))

(define (llama-cpp-shepherd-services config)
  (map llama-cpp-model->shepherd-service
       (llama-cpp-configuration-models config)))

(define llama-cpp-service-type
  (service-type
    (name 'llama-cpp)
    (extensions
     (list (service-extension account-service-type
                              (const %llama-cpp-accounts))
           (service-extension shepherd-root-service-type
                              llama-cpp-shepherd-services)))
    (default-value
      (llama-cpp-configuration
       (models (list (llama-cpp-model
                      (name "lfm2-8b")
                      (model-path "/data/models/LFM2-8B-A1B-GGUF/LFM2-8B-A1B-Q8_0.gguf")
                      (port "9090"))))))
    (description "Llama.cpp inference server. Each model gets its own shepherd service and log file.")))
