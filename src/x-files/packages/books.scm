(define-module (x-files packages books)
  #:use-module (gnu packages)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)

  #:use-module ((guix licenses)
                #:select (expat))
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system copy))

(define-public jrm-syntax-rules
  (let [(commit "6968df3ebc6de4cf8d41aa7b6adc400a11937b86")
        (hash "1l8221q1zw66yb47dcqbbnj3dbsl6slvnivaqc731hs848apwffr")
        (revision "1")]
    (package
      (name "jrm-syntax-rules")
      (version "1.0.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/shegeley/jrm-syntax-rules")
                      (commit commit)))
                (sha256 (base32 hash))
                (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (native-inputs (list gzip texinfo))
      (arguments
       (list #:install-plan ''(("jrm-syntax-rules.info" "share/info/"))
             #:phases #~(modify-phases
                            %standard-phases
                          (add-before
                              'install 'build
                            (lambda _
                              (invoke "makeinfo"
                                      "--output=jrm-syntax-rules.info"
                                      "doc.texi"))))))
      (home-page "https://github.com/shegeley/jrm-syntax-rules")
      (synopsis "JRM’s Syntax-rules Primer for the Merely Eccentric")
      (description "Tutorial for syntax-rules system in R5RS standard.")
      ;; https://funcall.blogspot.com/2023/10/syntax-rules-primer.html
      (license expat))))
