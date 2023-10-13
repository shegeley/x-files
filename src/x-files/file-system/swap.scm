(define-module (x-files file-system swap)
  #:use-module (x-files utils base)
  #:use-module (x-files utils string)

  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gnu system file-systems)

  #:use-module ((guix build utils) #:select
                (invoke))

  #:use-module (guix records)

  #:export (get-swaps
            swapfile
            create-swapfile!))

(define (get-swaps)
  "Returns the list of elements in \"/proc/swap\" parsed in the following format:
   @example
   ((\"Filename\" \"Type\" \"Size\" \"Used\" \"Priority\")
    (\"/swap\" \"file\" \"8388604\" \"0\" \"-2\"))
   @end example"
  (call-with-input-file "/proc/swaps"
    (lambda (p)
      (let* [(text (get-string-all p))
             (regex "[ \t]+")
             (rows (map (lambda (t) (string-split-regexp t regex))
                        (drop-right (string-split text #\newline) 1)))]
        rows))))

(define (swapfile? filename)
  "Returns swapfile entry in (get-swaps) if it exists, or @code{#f} otherwise"
  (find (lambda (x)
          (match x
            ((filename* type rest ...)
             (and (equal? type "file")
                  (equal? filename* filename)))
            (_ #f)))
        (drop (get-swaps) 1)))

(define* (create-swapfile!
          fs size
          #:optional (swapfile "swap"))
  "Create the @code{swapfile} with the given @code{fs} (filesystem) and @code{size} in bytes and set's up the swap on it. If the file al"
  (let [(file (string-append (file-system-mount-point fs) swapfile))]
    (cond
     ((and (file-exists? file)
           (equal? size (stat:size (stat file)))
           (swapfile? file))
      (format #t "Swapfile of the given size already exists. Skip (re)creating~%"))
     ((and (file-exists? file)
           (swapfile? file))
      (invoke "swapoff" file)
      (create-swapfile! fs size swapfile))
     ((file-exists? file)
      (delete-file file)
      (create-swapfile! fs size swapfile))
     (else
      ;; https://superuser.com/a/1616342
      (invoke "touch" file)
      (invoke "chattr" "+C" file) ;; NOTE: only works on cow-supporting file-systems. I don't know if it the invoke will fail on non-cows
      (invoke "fallocate" "-l" (number->string size) file)
      (invoke "mkswap" file)
      (chmod file 600)
      (invoke "chown" "root" file)
      (invoke "swapon" file)))))
