(define-module (x-files packages glab)
  #:use-module ((guix packages) #:select (package origin base32
                                           %current-system
                                           %current-target-system))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((nonguix build-system binary) #:select (binary-build-system))
  #:export (glab))

;; glab — the official GitLab CLI (gitlab-org/cli), a Go program not yet in
;; Guix proper.  Upstream ships statically-linked Go binaries (`file` reports
;; "statically linked, stripped") for several targets, so unlike chromedriver
;; we just unpack the release tarball — no patchelf / rpath surgery needed.
;; Both x86_64-linux and aarch64-linux tarballs share the same top-level
;; layout (bin/glab, CHANGELOG.md, LICENSE, README.md), so one install-plan
;; covers both.
;;
;; To bump: set %glab-version and recompute each hash with
;;   guix download https://gitlab.com/gitlab-org/cli/-/releases/vX.Y.Z/downloads/glab_X.Y.Z_linux_amd64.tar.gz
;;   guix download https://gitlab.com/gitlab-org/cli/-/releases/vX.Y.Z/downloads/glab_X.Y.Z_linux_arm64.tar.gz
(define %glab-version "1.112.0")

(define target->arch
  '(("x86_64-linux"  . "amd64")
    ("aarch64-linux" . "arm64")))

(define targets (map car target->arch))

(define target->hash
  '(("x86_64-linux"  . "1gwfda7zr6s8z60ill0b4ia6xl1w6myph5dn68h5yrlbal3jkigi")
    ("aarch64-linux" . "1y6qv26aa3x0x067s7vklzv6x7p550f5cpn5a50y68yvw1yzx9al")))

(define-public glab
  (let* [(target (or (%current-target-system) (%current-system)))
         (arch   (assoc-ref target->arch target))
         (hash   (assoc-ref target->hash target))]
    (package
      (name "glab")
      (version %glab-version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://gitlab.com/gitlab-org/cli/-/releases/v" %glab-version
               "/downloads/glab_" %glab-version "_linux_" arch ".tar.gz"))
         (sha256
          (base32 hash))))
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
                ;; The release tarball has several top-level entries (bin/,
                ;; LICENSE, README, CHANGELOG).  The stock 'unpack chdirs into
                ;; the first subdirectory it finds (bin/), which would make the
                ;; "bin/glab" install plan unreachable.  Extract and stay at the
                ;; top so the plan's paths are stable.
                (invoke "tar" "--extract" "--file" source))))))
      (supported-systems targets)
      (home-page "https://gitlab.com/gitlab-org/cli")
      (synopsis "GitLab CLI (glab)")
      (description
       "@code{glab} is the official GitLab command-line client.  It works with
merge requests, issues, pipelines, releases and the raw GitLab API, against
gitlab.com or a self-hosted instance selected via the @env{GITLAB_HOST}
environment variable.  This package installs the upstream statically-linked
release binary for the target architecture (x86_64 or aarch64).")
      (license license:expat))))
