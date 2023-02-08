(define-module (x-files utils files)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (guix build utils)

  #:export (-home-
            %trash-dir
            trash!
            git-project-dir
            file-format
            file-format=?
            *symlink*
            icon?
            desktop-entry?))

(define* (-home-
          #:optional (x ""))
  (string-append
   (or (getenv "HOME")
       (string-append "/home/" (getlogin)))
   "/" x))

(define (up dir)
  (string-append dir "/.."))

(define %trash-dir
  (-home- ".local/share/Trash/files/"))

(define (trash! f)
  (rename-file f (string-append %trash-dir (basename f))))

(define (file-format path)
  (last (string-split (basename path) #\.)))

(define (file-format=? path x)
  (equal? (file-format path) x))

(define* (*symlink* source dir
                    #:key (logging-port #t))
  (let ((o (string-append
            dir "/" (basename source))))
    (cond [(and (file-exists? o)
                (symbolic-link? o)
                (equal? (readlink o) source))
           ;; everything's fine
           (begin
             (format logging-port "No link replacement needed for ~s ~%" o)
             #t)]
          [(file-exists? o)
           ;; broken link
           (begin
             (format logging-port "The link ~s is broken. Removing ~%" o)
             (delete-file o))]
          [else
           (begin
             (format logging-port "The link ~s was not found. Symlinking with ~s ~%" o source)
             (symlink source o))])))

(define (desktop-entry? x)
  (file-format=? x "desktop"))

(define (icon? x)
  (fold (lambda (x y) (or x y))
        #f
        (map (cut file-format=? x <>)
             '("png" "svg"))))
