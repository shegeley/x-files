(define-module (x-files packages emacs clickhouse)
  #:use-module ((guix licenses)        #:prefix license:)
  #:use-module ((guix packages)        #:select (package origin base32))
  #:use-module ((guix git-download)    #:select (git-fetch git-reference git-file-name git-version))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system)))

(define-public emacs-sql-clickhouse
  (let [(commit "8403a4a5d332dbb6459b7fbce6ea95c36d390a5b")
        (version "0.0.1")]
    (package
      (name "emacs-sql-clickhouse")
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rschwarz/sql-clickhouse")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "0c039m67jc2xq9pmn0xmqr07nzdssc4vbk51ng0272kcs9mbslrf"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (home-page "https://github.com/rschwarz/sql-clickhouse")
      (synopsis "ClickHouse support for Emacs SQL mode")
      (description
       "Adds ClickHouse as a product to Emacs's built-in @code{sql-mode}:
syntax highlighting for ClickHouse SQL and an interactive session via the
@code{clickhouse-client} CLI (@code{M-x sql-clickhouse}).")
      (license license:gpl3+))))
