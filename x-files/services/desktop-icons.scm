(define-module (x-files services desktop-icons)
  #:use-module (srfi srfi-1)

  #:use-module (guix build utils)

  #:use-module (x-files utils base)
  #:use-module (x-files utils files)

  #:use-module (gnu services)
  #:use-module (gnu home services)

  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:export (desktop-icons-service-type
            desktop-icons-service-conf))

(define-record-type* <desktop-icons-service-conf>
  desktop-icons-service-conf
  desktop-icons-service-conf!
  desktop-icons-service-conf~
  desktop-icons-service-conf?
  ;; https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html
  (entries desktop-icons-service-conf:entries
           (default '())))

(define (activation config)
  (match-record
   config
   <desktop-icons-service-conf>
   (entries)
   (with-imported-modules
    (source-module-closure
     '((x-files utils files)
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
         (x-files utils files)
         (ice-9 match))

          (define (%home-path)
            (string-append (getenv "HOME")))

          (define (%icons-path)
            (string-append
             (%home-path)
             "/.local/share/icons"))

          (define (%desktop-entries-path)
            (string-append
             (%home-path)
             "/.local/share/applications"))

          (define (install-desktop-entry desktop-entry)
            (let ((f (string-append
                      (%desktop-entries-path)
                      "/" (basename desktop-entry))))
              (begin
                (*symlink* desktop-entry (%desktop-entries-path))
                (false-if-exception
                 (chmod f #o755)))))

          (define (install-icon icon)
            (*symlink* icon (%icons-path)))

          #$(match entries
              ((icons desktop-entries)
               #~(begin
                 (map install-icon (quasiquote #$icons))
                 (map install-desktop-entry (quasiquote #$desktop-entries)))))))))

(define-public desktop-icons-service-type
  (service-type
   (name 'icons-manager)
   (extensions
    (list
     (service-extension home-activation-service-type activation)))
   (default-value (desktop-icons-service-conf))
   (description
    "A simple service to add desktop entries (.desktop) and their icons (.svg, .png ...) to gnome menu.")))
