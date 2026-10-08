(define-module (x-files packages nix)
  #:use-module ((guix packages) #:select (package origin base32))
  #:use-module ((guix download) #:select (url-fetch))
  #:use-module ((guix gexp) #:select (gexp local-file file-append))
  #:use-module ((guix modules) #:select (source-module-closure))
  #:use-module ((guix build-system trivial) #:select (trivial-build-system))
  #:use-module ((guix licenses) #:select (expat lgpl2.1+))
  #:use-module ((gnu packages compression) #:select (xz zstd))
  #:use-module ((gnu packages search) #:select (recoll-cli))
  #:export (nix-manuals nix-manuals-index))

(define (html-source url hash name)
  (origin
    (method url-fetch)
    (uri url)
    (file-name name)
    (sha256 (base32 hash))))

(define %nix-manual
  (html-source
   "https://nix.dev/manual/nix/2.35/print.html"
   "1fdfyah2skc25hw340572d383dqdg9danl659gxkpqfc05zqpa1c"
   "nix-2.35.2.html"))

(define %nixpkgs-manual
  (html-source
   "https://nixos.org/manual/nixpkgs/unstable/"
   "0fkf3znwixj4hyc3fifqjv39g0s6v0224jlzhkqh563wskvs8b6f"
   "nixpkgs-65179426c83b.html"))

;; Published HTML closures from each channel, not a Nix build inside Guix.
(define %nixos-manuals
  '(("26.05" "zst"
     "13zm7ia3ndlkapnxir7xa9gzasvq3x6c0ccqarg7db9nh2ld0n4y"
     "0mdi2p46zg1sqzflafsws1m6bmm9b3xf2fzd71yqz5zb27g3mg80")
    ("25.11" "zst"
     "092yyp9r30vrngwfaqgj1cr74kpal67zsyw3hsy3gk7cridbx2jv"
     "0j0nhiq2nnfdqcx1wh74gp73w83my3w5bzls6d7jvw0f8yddmc5s")
    ("25.05" "xz"
     "09vaj9g4dkq5s0yrkz2ws0c38p7r5n5k0jrr1ij4xnfmamm3hkbq"
     "09vaj9g4dkq5s0yrkz2ws0c38p7r5n5k0jrr1ij4xnfmamm3hkbq")))

(define nix-manuals
  (package
    (name "nix-manuals")
    (version "2026-10-08")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules (source-module-closure
                 '((guix build utils) (guix serialization)))
      #:builder
      #~(begin
          (use-modules
           ((guix build utils) #:select (mkdir-p copy-recursively))
           ((guix serialization) #:select (restore-file))
           ((ice-9 popen) #:select (open-pipe* close-pipe))
           ((ice-9 match) #:select (match))
           ((sxml simple) #:select (sxml->xml)))
          (define directory (string-append #$output "/share/doc/nix-manuals"))
          (mkdir-p directory)
          ;; mdBook print.html files concatenate the whole book into one
          ;; multi-megabyte page, which shr/EWW renders pathologically
          ;; slowly.  Split them into one page per heading instead.
          (load #$(local-file (search-path %load-path
                               "x-files/packages/aux/nix-manuals/split-mdbook.scm")))
          (split-mdbook-print #$%nix-manual
                              (string-append directory "/nix") 1
                              #:book-title "Nix 2.35.2")
          (split-mdbook-print #$%nixpkgs-manual
                              (string-append directory "/nixpkgs") 3
                              #:book-title "Nixpkgs unstable")
          (for-each
           (lambda (entry)
             (match entry
               ((version decoder archive)
                (let* ((temporary (string-append (getcwd) "/nixos-" version))
                       (port (open-pipe* "r" decoder "-dc" archive)))
                  (restore-file port temporary)
                  (unless (zero? (close-pipe port))
                    (error "Cannot decompress manual" version))
                  (copy-recursively
                   (string-append temporary "/share/doc/nixos")
                   (string-append directory "/nixos-" version))))))
           (list
            #$@(map
                (lambda (entry)
                  (let ((version (list-ref entry 0))
                        (compression (list-ref entry 1))
                        (archive (list-ref entry 2))
                        (hash (list-ref entry 3)))
                    #~(list #$version
                            #$(if (string=? compression "xz")
                                  (file-append xz "/bin/xz")
                                  (file-append zstd "/bin/zstd"))
                            #$(html-source
                               (string-append "https://cache.nixos.org/nar/"
                                              archive ".nar." compression)
                               hash
                               (string-append "nixos-" version ".nar." compression)))))
                %nixos-manuals)))
          (call-with-output-file (string-append directory "/index.html")
            (lambda (port)
              (sxml->xml
               `(html
                 (head (meta (@ (charset "utf-8")))
                       (title "Руководства Nix и NixOS"))
                 (body
                  (h1 "Руководства Nix и NixOS — офлайн")
                  (p "Снимок 01.10.2026. Поиск: Ctrl-f в браузере, C-s в EWW.")
                  (ul
                   (li (a (@ (href "nix/index.html")) "Nix 2.35.2: язык и команды"))
                   (li (a (@ (href "nixpkgs/index.html")) "Nixpkgs unstable: рецепты и библиотека"))
                   ,@(map
                      (lambda (version)
                        `(li ,(string-append "NixOS " version ": ")
                             (a (@ (href ,(string-append "nixos-" version "/index.html"))) "руководство")
                             " · "
                             (a (@ (href ,(string-append "nixos-" version "/options.html"))) "параметры")
                             " · "
                             (a (@ (href ,(string-append "nixos-" version "/release-notes.html"))) "изменения")))
                      '("26.05" "25.11" "25.05")))
                  (p "Версии архивов фиксированы хешами. Руководства на английском.")))
               port))))))
    (home-page "https://nixos.org/learn/")
    (synopsis "Offline HTML manuals for Nix and recent NixOS releases")
    (description
     "Official HTML manuals for Nix 2.35.2, Nixpkgs and NixOS 26.05,
25.11 and 25.05, including the NixOS option reference and release notes.
The mdBook manuals (Nix and Nixpkgs) are split into one page per heading so
that EWW can render them quickly; intra-book links are rewritten to the page
containing their target.  Open share/doc/nix-manuals/index.html in EWW or a
web browser.  No Nix, Python, documentation converter or network connection
is needed to read them.
The rolling Nixpkgs and Nix manual URLs are guarded by snapshot hashes;
updating the snapshot requires updating those hashes.")
    (license (list expat lgpl2.1+))))

(define nix-manuals-index
  (package
    (inherit nix-manuals)
    (name "nix-manuals-index")
    (arguments
     (list
      #:modules (source-module-closure '((guix build utils)))
      #:builder
      #~(begin
          (use-modules
           ((guix build utils) #:select (mkdir-p invoke))
           ((ice-9 format) #:select (format)))
          (let ((config (string-append #$output "/share/nix-manuals-recoll"))
                (settings
                 `((topdirs . ,(string-append #$nix-manuals "/share/doc/nix-manuals"))
                   (dbdir . "xapiandb")
                   (indexedmimetypes . "text/html")
                   (indexallfilenames . 0)
                   (textfilemaxmbs . 100)
                   (noaspell . 1)
                   (indexstemminglanguages . "")
                   (loglevel . 1)
                   (logfilename . "stderr"))))
            (mkdir-p config)
            (call-with-output-file (string-append config "/recoll.conf")
              (lambda (port)
                (for-each
                 (lambda (entry)
                   (format port "~a = ~a~%" (car entry) (cdr entry)))
                 settings)))
            ;; Disable the default external Joplin/Python indexer.
            (call-with-output-file (string-append config "/backends")
              (lambda (port) (display "" port)))
            (invoke #$(file-append recoll-cli "/bin/recollindex") "-c" config)
            (invoke #$(file-append recoll-cli "/bin/recollq")
                    "-c" config "-n" "1" "-F" "url" "overrideAttrs")))))
    (synopsis "Prebuilt Recoll full-text index of the offline Nix manuals")
    (description
     "Recoll/Xapian index of nix-manuals, built once in the Guix store.
Queries use share/nix-manuals-recoll as their Recoll configuration directory.
Only the packaged HTML manuals are indexed; no user files or background
indexing service are involved.")))

nix-manuals
