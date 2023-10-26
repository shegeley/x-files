(define-module (x-files utils project)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)

  #:export (git-project-dir))

(define* (git-project-dir
          #:optional (start-dir (getcwd)))
  "Returns an absolute path: first upmost directory contaning \".git\" directory relative to the current process [via @code{cwd}]. Or false if one not found going too far up the filetree"
  (let* ((up (lambda (x) (string-append x "/../")))
         (start-dir (canonicalize-path start-dir))
         (split (string-split start-dir #\/)))
    (match split
      (("" "home" x) #f)
      (("" "home" x "") #f)
      (("" "home" x ...)
       (let* ((scan
               (scandir start-dir
                        (lambda (x) (equal? x ".git")))))
         (match scan
           ('(".git") start-dir)
           ('() (git-project-dir
                 (up start-dir))))))
      (_ #f))))
