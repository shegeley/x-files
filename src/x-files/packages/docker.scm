(define-module (x-files packages docker)
  #:use-module (gnu)
  #:use-module (guix packages)

  #:use-module (gnu packages docker)

  #:export (docker/latest
            docker-cli/latest
            docker-compose/latest
            containerd/latest))

(define docker/latest
  (package
    (inherit docker)
    (version "24.0.7")))

(define docker-cli/latest
  (package
    (inherit docker-cli)
    (version "23.0.8")))

(define docker-compose/latest
  (package
    (inherit docker-compose)
    (version "2.23.3")))

(define containerd/latest
  (package
    (inherit containerd)
    (version "1.7.11")))
