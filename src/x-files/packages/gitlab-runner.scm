(define-module (x-files packages gitlab-runner)
  #:use-module (guix gexp)
  #:use-module ((guix packages) #:select (package
                                          origin))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix base16) #:select (base16-string->bytevector))
  #:use-module ((guix utils) #:select (%current-system
                                       %current-target-system))
  #:use-module ((guix build-system trivial) #:select (trivial-build-system))
  #:use-module ((guix licenses) #:select (expat))
  #:export (gitlab-runner-bin))

(define gitlab-runner-bin
  (let* ((system (or (%current-target-system) (%current-system)))
         (release (assoc-ref
                   '(("x86_64-linux"
                      "amd64"
                      "0e11e71404352e67d286269aee397ec8e2825e0fc91a12f60c04cef0b85edb85")
                     ("aarch64-linux"
                      "arm64"
                      "bc002aa164508682664b761ee737a992f3cae1ae085469d7667870dfa30c2301"))
                   system)))
    (unless release
      (error "Unsupported GitLab Runner system" system))
    (package
      (name "gitlab-runner-bin")
      (version "19.5.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://gitlab-runner-downloads.s3.amazonaws.com/v"
                             version "/binaries/gitlab-runner-linux-" (car release)))
         (sha256 (base16-string->bytevector (cadr release)))))
      (build-system trivial-build-system)
      (arguments
       (list #:builder
             #~(begin
                 (mkdir #$output)
                 (mkdir (string-append #$output "/bin"))
                 (let ((target (string-append #$output "/bin/gitlab-runner")))
                   (copy-file #$source target)
                   (chmod target #o555)))))
      (supported-systems '("x86_64-linux"
                           "aarch64-linux"))
      (home-page "https://gitlab.com/gitlab-org/gitlab-runner")
      (synopsis "GitLab CI job runner (upstream static binary)")
      (description "@command{gitlab-runner} executes CI jobs and reports their results to
GitLab.  This package pins the official static binary and its release checksum;
it does not rebuild the Go source.  Docker executor helper images are separate.")
      (license expat))))
