(define-module (x-files utils flatpak)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (x-files utils files)

  #:export (entries))

(define (flatpak-local-dir)
  (string-append
   (getenv "HOME")
   "/.local/share/flatpak"))

(define* (entries #:optional
                  (dir (flatpak-local-dir)))
  "Finds all entries of currently installed flatpak apps in `dir`
   Returns a list of '(icons desktop-entries)
   Where icon := (or .png .svg) file, desktop-entry := .desktop file"
  (let ((m (string-append dir "/app"))
        (icons '())
        (entries '()))
    (ftw m
         (lambda (filename statinfo flag)
           (let* ((k
                   (string-split
                    (string-drop filename (string-count m char?)) #\/)))
             (match (last k)
               ((? icon?)
                (set! icons (append icons (list filename)))
                #t)
               (_ #t))
             (match k
               (("" app-id "current" "active"
                 "export" "share"
                 "applications" desktop-entry)
                (if (desktop-entry? desktop-entry)
                    (set! entries
                          (append entries (list filename)))
                    #t)
                #t)
               (_ #t)))))
    (list icons entries)))
