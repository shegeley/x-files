(define-module (x-files services dotfiles)
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

(define* (sync-package-files storage scheme)
  (map
   (match-lambda
     ((home-dir-loc config-dir-loc)
      (let ((home-dir-loc (-home- home-dir-loc))
            (config-dir-loc (storage config-dir-loc)))
        #~(begin
            (format #t "Symlinking ~a with ~a ~%" #$home-dir-loc #$config-dir-loc)
            (cond ((and (file-exists? #$home-dir-loc)
                        (not (file-exists? #$config-dir-loc)))
                   (begin
                     (rename-file #$home-dir-loc #$config-dir-loc)
                     (symlink #$config-dir-loc #$home-dir-loc)
                     (format #t "Dir ~s was not found. Moving ~s to config dir and symlinking. ~%" #$config-dir-loc #$home-dir-loc)))
                  ((and (file-exists? #$home-dir-loc)
                        (file-exists? #$config-dir-loc))
                   (begin
                     (delete-file-recursively #$home-dir-loc)
                     (symlink #$config-dir-loc #$home-dir-loc)
                     (format #t "File ~s was found. Deleting it and symlinking it to ~s. ~%"  #$home-dir-loc #$config-dir-loc)))
                  ((and (not (file-exists? #$home-dir-loc))
                        (file-exists? #$config-dir-loc))
                   (begin
                     (symlink #$config-dir-loc #$home-dir-loc)
                     (format #t "File ~s was not found. Symlinking it with ~s. ~%"  #$home-dir-loc #$config-dir-loc)))
                  ((and (not (file-exists? #$home-dir-loc))
                        (not (file-exists? #$config-dir-loc)))
                   (format #t "File/Directory ~s doesn't exists, neither file/directory ~s exists. Can't synchronize these dotfiles. ~%" #$home-dir-loc #$config-dir-loc))
                  (else
                   (format #t "Don't know how to handle this case syncronizing ~s and ~s. ~%" #$home-dir-loc #$config-dir-loc))))))) scheme))

(define (activation config)
  (match-record config <dotfile-manager-conf>
    (storage schemes)
    (let ((schemes (normalize-schemes schemes)))
      (with-imported-modules
          (source-module-closure
           '((x-files utils files)
             (ice-9 format)
             (ice-9 match))
           #:select?
           (lambda (name)
             (or
              ((@@ (guix modules) guix-module-name?) name)
              (match name
                (('x-files 'utils ..) #t)
                (else #f)))))
        #~(begin
            (use-modules
             (ice-9 match)
             (ice-9 format)
             (x-files utils files))

            #$@(map
                (match-lambda
                  ((package scheme)
                   #~(if (member #$package
                                 (quasiquote #$%keys-packages))
                         (format #t "Don't know how to synchronize keys yet. TODO: Think about it. ~%")
                         #$@(sync-package-files storage scheme))))
                schemes)

            (format #t "Done syncronizing dotfiles. ~%"))))))

(define-public dotfile-manager-service-type
  (service-type
   (name 'dotfile-manager)
   (extensions
    (list (service-extension home-activation-service-type activation)))
   (description
    "Simple service to keep up all your dotfiles. In general this is very dirty and quick solution. It'd be better to manage them as gexp-generated. TODO.")))
