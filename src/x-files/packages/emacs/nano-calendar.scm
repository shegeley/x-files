(define-module (x-files packages emacs nano-calendar)
  #:use-module ((guix licenses)        #:prefix license:)
  #:use-module ((guix packages)        #:select (package origin base32))
  #:use-module ((guix git-download)    #:select (git-fetch git-reference git-file-name git-version))
  #:use-module ((guix build-system emacs) #:select (emacs-build-system)))

(define-public emacs-nano-calendar
  (let [(commit "bdd7e001ecf636adaf27c5cf707ca987b76608de")
        (version "1.0")]
    (package
      (name "emacs-nano-calendar")
      (version (git-version version "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rougier/nano-calendar")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "1ia2v7810ibp8ac4k7xwdwnr4qa601haycy10mh5nkpnd73cdq61"))))
      (build-system emacs-build-system)
      (arguments (list #:tests? #f))
      (home-page "https://github.com/rougier/nano-calendar")
      (synopsis "Interactive calendar widget for Emacs")
      (description
       "Displays a calendar in the current buffer and allows selecting one
or several dates.  Built on Emacs's own @code{calendar}, @code{holidays}
and @code{org-agenda} libraries, with no external elisp dependencies.")
      (license license:gpl3+))))
