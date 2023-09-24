(define-module (x-files utils files)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)

  #:use-module (guix build utils)

  #:use-module ((guix build syscalls)
                #:select
                (free-disk-space))

  #:use-module ((gnu services configuration)
                #:select (interpose))

  #:export (-home-
            %trash-dir
            trash!
            file-format
            file-format=?
            *symlink*
            icon?
            -storage-
            -ssh-key-
            fs-path->symlist
            symlist->fs-path
            desktop-entry?
            size
            enough-space?
            safe-copy-recursively
            safe-move
            safe-move&symlink))

(define (fs-path->symlist path)
  "Simple function that splits file-system path to list of symbols
   @example
   (fs-path->symlist \"/home/user/documents/doc1.txt\") =>
     '(home user documents doc1.txt)
   @end examples"
  (let [(l (string-split path #\/))]
    (fold-right
     (lambda (x acc)
       (if (equal? "" x)
           acc
           (cons (string->symbol x) acc)))
     '() l)))

(define (symlist->fs-path symlist)
  "Simple function that concats list of symbols to the file-path
   @example
   (symlist->fs-path `(home user Documents)) =>
     \"/home/user/Documents\"
   @end examples"
  (apply
   string-append "/"
   (interpose (map symbol->string symlist) "/")))

(define* (-home-
          #:optional (x ""))
  (let [(guess (string-append
                (or (getenv "HOME")
                    (string-append "/home/" (getlogin)))
                "/" x))]
    (if (directory-exists? guess) guess #f)))

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

(define* (-storage- #:optional (x ""))
  (false-if-exception
   (string-append (getenv "STORAGE") "/" x)))

(define* (-ssh-key- #:optional (x ""))
  (false-if-exception
   (string-append
    (or (string-append (getenv "HOME") "/.ssh")
        (getenv "SSH_STORAGE")) "/" x)))

(define (size src)
  (if (file-exists? src)
      (stat:size (stat src)) #f))

(define* (enough-space?
          where how-much
          #:key (offset (expt 1024 3)))
  (>= (+ (free-disk-space where) offset) how-much))

(define* (safe-copy-recursively
          src dst
          #:key
          (port (current-output-port))
          offset
          #:allow-other-keys
          #:rest args)
  (let [(size* (size src))]
    (cond
     ((file-exists? dst)
      (error (format #f "Can't copy ~a to ~a! ~a already exsits!~%" src dst dst)))
     ((not (enough-space? dst size*))
      (error
       (format #f "Not enough space on ~a! ~a bytes needed but only ~a avalialiable with offset ~a ~%" dst size* (size dst) offset)))
     (else
      (apply copy-recursively src dst args)))))

(define (safe-move src dst)
  (when (file-exists? src)
    (safe-copy-recursively src dst)
    (delete-file-recursively src)))

(define* (safe-move&symlink
          src dst
          #:key (port (current-output-port)))
  (cond ((and (symbolic-link? src)
              (equal? dst (canonicalize-path src)))
         (format port "Symlink ~a already exists and it points to ~a~%" src dst))
        (else
         (safe-move src dst)
         (symlink dst src))))
