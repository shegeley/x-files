(define-module (x-files packages emacs dslide)
  #:use-module ((guix licenses)           #:prefix license:)
  #:use-module ((guix packages)           #:select (package origin base32))
  #:use-module ((guix git-download)       #:select (git-fetch git-reference
                                                            git-file-name
                                                            git-version))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system))

  #:export (emacs-dslide))

;; Upstream tags no releases ("unstable" branch carries development); master
;; is a commit past the 0.6.1 bump whose dslide.el header already declares
;; 0.6.2.
(define emacs-dslide
  (let [(url    "https://github.com/positron-solutions/dslide")
        (commit "be47f2dcb939779067f8c77c3493162bcf242b83")
        (version "0.6.2")
        (hash   "0kznpg734vq4k0gz3scm176ak7z0pm7hc19mbnf7n0bba6pnybr5")]
    (package
      (name "emacs-dslide")
      (version (git-version version "1" commit))
      (source
       (origin
        (method git-fetch)
        (uri (git-reference (url url) (commit commit)))
        (file-name (git-file-name name version))
        (sha256 (base32 hash))))
      (build-system emacs-build-system)
      (home-page url)
      (synopsis "Declarative slides from org mode headings")
      (description
       "DSlide presents anything Emacs can do with programmable, extensible,
configurable slides & presentation steps made from org mode headings.  Slides
are declared with org properties and can run elisp, org babel, and keyboard
macros as presentation steps, so live demos of real Emacs workflows become
part of the deck.  It is a descendant of org-tree-slide.")
      (license license:gpl3+))))
