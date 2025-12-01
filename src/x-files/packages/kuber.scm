(define-module (x-files packages kuber)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)

  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

;; stolen from @abcdw
;; https://git.sr.ht/~abcdw/cons.town/tree/master/item/src/guile/cons/guix/packages/kubernetes.scm

(define-public k0s
  (let ((version-suffix "+k0s.0"))
    (package
      (name "k0s")
      (version "1.31.3")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/k0sproject/k0s/releases/download/v"
                      version version-suffix
                      "/k0s-v" version version-suffix "-amd64"))
                (sha256 (base32 "0nbks656j1m42dws18mm7qw2np2fqrziqq16av54mh3s4dxs2m7d"))))
      (native-inputs (list binutils coreutils))
      (build-system trivial-build-system)
      (arguments
       (list
        #:modules '((guix build utils))
        #:builder
        #~(begin
            (use-modules (guix build utils))
            (let* ((src #$(package-source this-package))
                   (out #$output)
                   (path (string-append out "/bin/k0s")))
              (mkdir-p (string-append out "/bin"))
              (copy-recursively src path)
              (chmod path #o755)))))
      (supported-systems '("x86_64-linux"))
      (synopsis "The Zero Friction Kubernetes")
      (description
       "@code{k0s} is an all-inclusive Kubernetes distribution, which is
 configured with all of the features needed to build a Kubernetes
 cluster and packaged as a single binary for ease of use. ")
      (home-page "https://k0sproject.io/")
      (license license:apsl2))))

(define-public kubectl
  (package
    (name "kubectl")
    (version "1.30.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.k8s.io/release/v"
                                  version "/bin/linux/amd64/kubectl"))
              (sha256
               (base32
                "1rf79s2m1d953aprcchg4vz6iw26ni9cirj36rn2csr3plb3in5b"))))
    (build-system trivial-build-system)
    (native-inputs `(("source" ,source)))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((bin (string-append #$output "/bin"))
                 (kubectl (string-append bin "/kubectl")))
            (mkdir-p bin)
            (copy-file #$(this-package-native-input "source") kubectl)
            (chmod kubectl #o555)))))
    (home-page "https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/")
    (synopsis "Kubernetes CLI")
    (description "This package provides a Kubernetes CLI utility.")
    (license license:asl2.0)))

(define-public helm
  (package
    (name "helm")
    (version "3.16.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.helm.sh/helm-v"
                                  version "-linux-amd64.tar.gz"))
              (sha256
               (base32
                "1sllnavlrpg0gl3mxxv41b0g8wg042ws6cjl7k9fwl8935wmqdgm"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (bin     (string-append out "/bin"))
                (target  (string-append bin "/helm"))
                (gzip    (assoc-ref %build-inputs "gzip"))
                (tar     (assoc-ref %build-inputs "tar"))
                (source  (assoc-ref %build-inputs "source")))
           (setenv "PATH" (string-append gzip "/bin"))
           (invoke (string-append tar "/bin/tar") "xvf" source)
           (chdir "linux-amd64")
           (chmod "helm" #o555)
           (install-file "helm" bin)
           (install-file "LICENSE" (string-append out "/share/doc/"
                                                  ,name "-" ,version)))
         #t)))
    (native-inputs
     `(("gzip" ,gzip)
       ("source" ,source)
       ("tar"  ,tar)))
    (home-page "https://helm.sh/")
    (synopsis "Kubernetes Package Manager")
    (description "Helm is a tool for managing Kubernetes charts.  Charts are
packages of pre-configured Kubernetes resources.
Use Helm to:
@enumerate
@item Find and use popular software packaged as Helm charts to run in Kubernetes
@item Share your own applications as Helm charts
@item Create reproducible builds of your Kubernetes applications
@item Intelligently manage your Kubernetes manifest files
@item Manage releases of Helm packages
@end enumerate")
    (supported-systems '("x86_64-linux"))
    (license license:asl2.0)))

;; ((@ (rde api store) build-with-store) kubectl)
;; ((@ (rde api store) build-with-store) k0s)
