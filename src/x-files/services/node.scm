(define-module (x-files services node)
  #:use-module (srfi srfi-1)
  #:use-module (guix build utils)

  #:use-module (x-files utils base)
  #:use-module (x-files utils alist)
  #:use-module (x-files utils files)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages node)

  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)

  #:use-module (druix packages javascript yarn)

  #:use-module (gnu packages tree-sitter)

  #:use-module (contrib packages node-xyz)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format))

(define python-package-matcher
  `(("python" . ,python)))

(define config
  `(("script-shell" . "bash")
    ("python" . "python")
    ("prefix" . ,(string-append
                  (getenv "HOME") "/" "npm-packages"))))

(define (serialize config)
  (match config
    (((k . v) rest ...)
     (string-append k "=" v "\n"
                    (serialize rest)))
    ('() "")))

(define (profile config)
  (match-alist
   config
   (("python" python))
   (list bash
         (assoc-ref python-package-matcher python)
         tree-sitter-typescript
         tree-sitter-javascript
         node-typescript
         node-typescript-language-server
         ;; node-eslint-8.17.0
         node-lts
         yarn)))

(define (activation config)
  (match-alist
   config
   (("prefix" prefix))
   (with-imported-modules
       `((guix build utils))
     #~(begin
         (use-modules (guix build utils))

         (unless (directory-exists? #$prefix)
           (mkdir #$prefix))))))

(define (files config)
  `((".npmrc"
     ,(plain-file "npmrc"
                  (serialize config)))))

(define (envars! config)
  (match-alist
   config
   (("prefix" prefix))
   `(("PATH" . ,(string-append "$PATH:" prefix "/bin")))))

(define-public node-dev-service-type
  (service-type
   (name 'node-dev-service-type)
   (extensions
    (list
     (service-extension home-files-service-type
                        files)
     (service-extension home-environment-variables-service-type
                        envars!)
     (service-extension home-profile-service-type
                        profile)
     (service-extension home-activation-service-type
                        activation)))
   (default-value config)
   (description
    "Node development environment service type")))
