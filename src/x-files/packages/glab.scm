(define-module (x-files packages glab)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

;; glab — the official GitLab CLI (gitlab-org/cli), a Go program not yet in
;; Guix proper.  Upstream ships a statically-linked linux amd64 binary (`file`
;; reports "statically linked, stripped"), so unlike geckodriver/chromedriver
;; there is no rpath surgery — we just unpack the release tarball.
;;
;; To bump: set version and recompute the hash with
;;   guix download https://gitlab.com/gitlab-org/cli/-/releases/vX.Y.Z/downloads/glab_X.Y.Z_linux_amd64.tar.gz

(define-public glab
  (let* [(version "1.107.0")
         (uri     (string-append
                   "https://gitlab.com/gitlab-org/cli/-/releases/v" version
                   "/downloads/glab_" version "_linux_amd64.tar.gz"))]
    (package
      (name "glab")
      (version version)
      (source (origin
                (method url-fetch)
                (uri uri)
                (sha256
                 (base32
                  "0b5fslq91xgr05sk558jzkkz237n1vzn0nda497wz2d7n5pgahpb"))))
      (build-system binary-build-system)
      (arguments
       (list
        ;; tarball layout: bin/glab, CHANGELOG.md, LICENSE, README.md
        #:install-plan #~'(("bin/glab" "/bin/glab"))
        ;; already stripped; re-stripping a static Go binary is pointless.
        #:strip-binaries? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'unpack
              (lambda* (#:key source #:allow-other-keys)
                ;; The tarball has several top-level entries (bin/, LICENSE,
                ;; README, CHANGELOG).  The stock 'unpack chdirs into the first
                ;; subdirectory it finds (bin/), which makes the "bin/glab"
                ;; install plan unreachable.  Extract and stay at the top.
                (invoke "tar" "--extract" "--file" source))))))
      (supported-systems '("x86_64-linux"))
      (home-page "https://gitlab.com/gitlab-org/cli")
      (synopsis "GitLab CLI (glab)")
      (description
       "@code{glab} is the official GitLab command-line client.  It works with
merge requests, issues, pipelines, releases and the raw GitLab API, against
gitlab.com or a self-hosted instance selected via the @env{GITLAB_HOST}
environment variable.  This package installs the upstream statically-linked
linux amd64 release binary.")
      (license license:expat))))
