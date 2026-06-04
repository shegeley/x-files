(define-module (x-files packages etcd)
  #:use-module (ice-9 match)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)

  #:use-module (nonguix build-system binary)

  #:use-module ((guix licenses) #:prefix license:))

(define-public etcd-3.6.12
  (let* [(target (or (%current-target-system) (%current-system)))
         (architecture
          (string-append "linux-"
                         (match target
                           ("x86_64-linux"  "amd64")
                           ("powerpc64le-linux" "ppc64le")
                           ("aarch64-linux" "arm64"))))
         (hash
          (match target
            ("x86_64-linux"  "0m8kskaibmp4h1apj7vnvh6f410x39bakr5xyfx2zraq1fslnmnj")
            ("aarch64-linux" "1k49h631syw57k4f16pvv4xr3wq3i75bqqc5mm1a6kqn1rqzqyhl")))]
    (package
      (name "etcd")
      (version "3.6.12")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/etcd-io/etcd/releases/download/v"
               version "/etcd-v" version "-" architecture ".tar.gz"))
         (sha256 (base32 hash))))
      (build-system binary-build-system)
      (supported-systems '("x86_64-linux" "aarch64-linux" "powerpc64le-linux"))
      (arguments
       (list #:install-plan
             #~'(("etcd" "/bin/etcd")
                 ("etcdctl" "/bin/etcdctl"))))
      (home-page "https://etcd.io")
      (synopsis "etcd is a distributed reliable key-value store for the most critical data of a distributed system")
      (description "@code{etcd} is a distributed reliable key-value store for the most critical data of a distributed system, with a focus on being:

@itemize
@item Simple: well-defined, user-facing API (gRPC)
@item Secure: automatic TLS with optional client cert authentication
@item Fast: benchmarked 10,000 writes/sec
@item Reliable: properly distributed using Raft
@end itemize

@code{etcd} is written in Go and uses the Raft consensus algorithm to manage a highly-available replicated log")
      (license license:asl2.0))))

(define-public etcd etcd-3.6.12)
