(define-module (x-files services dotfiles)
  #:use-module (srfi srfi-1)

  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:use-module (guix build utils)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)

  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services shells)

  #:use-module (gnu packages shells)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages gnupg)

  #:use-module (gnu services configuration)

  #:use-module (gnu home services utils)

  #:export (dotfile-manager-conf
            <dotfile-manager-conf>
            dotfile-manager-service-type))

(define %keys-packages
  (map package-name (list gnupg openssh wireguard-tools)))

(define* (-storage- #:optional (x ""))
  (string-append (getenv "STORAGE") "/" x))

(define-record-type* <dotfile-manager-conf>
  dotfile-manager-conf
  dotfile-manager-conf!
  dotfile-manager-conf~
  dotfile-manager-conf?
  (schemes dotfile-manager-conf:schemes)
  (storage dotfile-manager-conf:storage
           (default -storage-)))

(define (normalize-schemes schemes)
  (map
   (match-lambda
     (((? package? package) schemes)
      (list (package-name package) schemes))
     (((? string? package) schemes)
      (list package schemes))) schemes))

(define (gen-name x)
  (let ((b (basename x)))
    (if (equal? "." (string-take b 1))
        (string-drop b 1)
        b)))

(define (sync-package-files storage scheme)
  (map
   (match-lambda
     ((home-dir-loc config-dir-loc)
      (let* ((config-dir-loc (storage config-dir-loc))
             (f (lambda (x) (local-file
                        x (gen-name x)
                        #:recursive?
                        (if (directory-exists? x)
                            #t #f)))))
        (list home-dir-loc (f config-dir-loc)))))
   scheme))

(define (files config)
  (match-record
   config <dotfile-manager-conf>
   (storage schemes)
   (let* ((schemes (normalize-schemes schemes))
          (acc '()))
     (map
      (match-lambda
        ((package scheme)
         (if (member package %keys-packages)
             (format #t "Don't know how to synchronize keys yet. TODO: Think about it. ~%")
             (set! acc (append acc (sync-package-files storage scheme))))))
      schemes)
     acc)))

(define-public dotfile-manager-service-type
  (service-type
   (name 'dotfile-manager)
   (extensions
    (list
     (service-extension home-files-service-type files)))
   (description
    "Simple service to keep up all your dotfiles. In general this is very dirty and quick solution. It'd be better to manage them as gexp-generated. TODO.")))
