(define-module (x-files packages emacs ob-nix)
  #:use-module ((guix packages)           #:select (package origin base32))
  #:use-module ((guix gexp)               #:select (gexp))
  #:use-module ((guix download)           #:select (url-fetch))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((gnu packages package-management) #:select (nix))

  #:export (emacs-ob-nix))

(define emacs-ob-nix
  (package
    (name "emacs-ob-nix")
    (version "0.01")
    (source
     (origin
       (method url-fetch)
       ;; Pinned commit tarball: git-fetch is unusable from this network --
       ;; port 22 to codeberg.org is unreachable.
       (uri "https://codeberg.org/theesm/ob-nix/archive/76d71b37fb031f25bd52ff9c98b29292ebe0424e.tar.gz")
       (sha256 (base32 "1c5s1lwssclsqnhmh2vdnkrawxw81ikmmvsh2sbq4gby5lj9s15p"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:tests? #f ;; no test suite in repo
      #:phases
      #~(modify-phases %standard-phases
          ;; Bake the absolute `nix-instantiate' store path into ob-nix.el so
          ;; babel evaluation never relies on $PATH.
          (add-after 'unpack 'patch-nix-instantiate-path
            (lambda* (#:key inputs #:allow-other-keys)
              (emacs-substitute-variables "ob-nix.el"
                ("ob-nix-command"
                 (search-input-file inputs "/bin/nix-instantiate"))))))))
    (inputs (list nix))
    (home-page "https://codeberg.org/theesm/ob-nix")
    (synopsis "Org-babel support for Nix expressions")
    (description
     "@code{ob-nix} adds a @code{nix} source block language to org-babel.
Block bodies are evaluated with @code{nix-instantiate --eval}; the header
arguments @code{:json}, @code{:xml} and @code{:strict} map to the
corresponding @code{nix-instantiate} flags.  @code{ob-nix-command} is patched
to the absolute Guix store path of @code{nix-instantiate} at build time.")
    (license license:gpl3+)))
