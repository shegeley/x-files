(define-module (x-files packages ntfy-el)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((gnu packages curl) #:select (curl))
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public emacs-ntfy
  (package
    (name "emacs-ntfy")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shombando/ntfy")
             (commit "d78077d7bea205a0012837801f1e0e7044cac402")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crbzbanacj6ayny3z4yp9qzrf2a2n8sv28pfypfhxd6bd337hpj"))))
    (build-system emacs-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'hardcode-curl
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "ntfy.el"
                ;; ntfy shells out to curl via `start-process'; pin it to the
                ;; store so it works regardless of the user's PATH.
                (("\"curl\"")
                 (string-append "\"" (search-input-file inputs "/bin/curl")
                                "\""))
                ;; curl is a system binary now hardcoded above, not an Emacs
                ;; package: drop the bogus Package-Requires entry.
                (("\\(\\(emacs \"27.2\"\\) \\(curl\\)\\)")
                 "((emacs \"27.2\"))")))))))
    (inputs (list curl))
    (home-page "https://github.com/shombando/ntfy")
    (synopsis "Send ntfy.sh push notifications from Emacs")
    (description
     "This package is an interface to the @url{https://ntfy.sh,ntfy} service
(or a self-hosted instance) for sending push notifications from Emacs.  It
provides interactive commands to publish ad-hoc messages, messages with custom
headers and tags, and URLs, all delivered via @command{curl}.")
    (license license:expat)))
