(define-module (x-files services mcp filesystem)
  #:use-module (x-files utils records)

  #:use-module (gnu services)
  #:use-module (guix gexp)

  #:use-module ((gnu home services) #:select (home-shepherd-service-type))
  #:use-module ((x-files packages mcp filesystem) #:select (rust-mcp-filesystem)))

(define (home-mcp-filesystem-shepherd-service _)
  (list
   (shepherd-service
     (provision '(mcp-filesystem))
     (requirement '(file-systems))
     (documentation "")
     (start
      #~(make-forkexec-constructor
         (list
          #$(file-append rust-mcp-filesystem
                         "/bin/rust-mcp-filesystem")
         #:environment-variables '#$%default-system-environment
         #:log-file #$(k0s-worker-log-file config)))
     (auto-start? #t)
     (stop #~(make-kill-destructor)))))

(define-public home-mcp-filesystem-service-type
  (service-type
   (name 'mcp-filesystem)
   (extensions
    (list
     (service-extension home-shepherd-service-type home-mcp-filesystem-shepherd-service)))
   (default-value '())
   (description "")))
