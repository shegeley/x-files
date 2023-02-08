(define-module (x-files packages cups)
  #:use-module (x-files packages cups drivers samsung)

  #:use-module (x-files utils base)
  #:use-module (x-files utils list)

  #:use-module (guix licenses)
  #:use-module (gnu services cups)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages)
  #:use-module (guix packages)

  #:use-module (guix build union)
  #:use-module (guix build utils)
  #:use-module (guix build copy-build-system)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)

  #:use-module (gnu packages gnome)

  #:use-module (oop goops)

  #:use-module (srfi srfi-1)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw))

(define mf->package
  ;; "Manufacturer->package"
  (match-lambda
    ('samsung
     samsung-drivers)))

(define install-cups-filters-symlinks
  `(lambda* (#:key inputs outputs #:allow-other-keys)
     (let ((out (assoc-ref outputs "out"))
           (cups-filters (assoc-ref inputs "cups-filters")))
       ;; Charsets.
       (symlink
        (string-append cups-filters "/share/cups/charsets")
        (string-append out "/share/charsets"))

       ;; MIME types, driver files, and PPDs.
       (for-each
        (lambda (f)
          (symlink (string-append cups-filters f)
                   (string-append out f)))
        '("/share/cups/mime/cupsfilters.types"
          "/share/cups/mime/cupsfilters.convs"
          "/share/cups/drv/cupsfilters.drv"
          "/share/ppd"))

       ;; Filters.
       (for-each
        (lambda (f)
          (symlink f
                   (string-append out "/lib/cups/filter/" ;; Add "/" here
                                  (basename f))))
        (find-files (string-append cups-filters "/lib/cups/filter")))

       ;; Backends.
       (for-each
        (lambda (f)
          (symlink (string-append cups-filters f)
                   (string-append out "/lib/cups/backend/"
                                  (basename f))))
        '("/lib/cups/backend/parallel"
          "/lib/cups/backend/serial"))

       ;; Banners.
       (let ((banners "/share/cups/banners"))
         (delete-file-recursively (string-append out banners))
         (symlink (string-append cups-filters banners)
                  (string-append out banners)))

       ;; Assorted data.
       (let ((data "/share/cups/data"))
         (delete-file-recursively (string-append out data))
         (symlink (string-append cups-filters data)
                  (string-append out data))))))

(define (install-symlinks mf)
  `(lambda* (#:key inputs outputs
             #:allow-other-keys)
     (let ((spd (assoc-ref inputs
                           ,(package-name (mf->package mf)))))
       ;; NOTE: have a look at directory (or file) union somewhere in guix utils
       (map
        (match-lambda
          ((a b)
           (let ((source (string-append spd "/" b))
                 (target (string-append (assoc-ref outputs "out") "/" b)))
             (cond ;; either directory of file
              ((directory-exists? source)
               (for-each
                (lambda (x)
                  (let ((target* (string-append target "/" (basename x))))
                    (symlink x target*)))
                (find-files source)))
              ((file-exists? source)
               (symlink source target))))))
        (quote ,(install-plan 'samsung))) ;; has to be quoted to treat it as 2-d list, not set of sexps (tries to apply them)
       #t)))

;; Look at /gnu/store/6h3a063krjh022ck76zgnqqa09axa47s-hplip-minimal-3.21.10/share/cups/drv/hp/hpcups.drv
;; /gnu/store/q12zapflna8bd8i3wy2vhcy81mlbgnkg-cups-server-bin/share/cups/drv/hp/hpcups.drv
;; /gnu/store/rl1r3p5401zl9mgdyf7z6nhp19hjp3lm-cups-2.3.3/share/cups/drv/sample.drv <- NOTABENE
;; https://wiki.elbrus.ru/%D0%A1%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_%D1%81%D0%BE%D0%B2%D0%BC%D0%B5%D1%81%D1%82%D0%B8%D0%BC%D0%BE%D1%81%D1%82%D0%B8/%D0%9F%D1%80%D0%B8%D0%BD%D1%82%D0%B5%D1%80%D1%8B/CUPS
;; and cups build process

(define-public (cups-filters* . mfs)
  (package
   (inherit cups-filters)
   (inputs
    (append
     (map
      (lambda (mf)
        (let ((p (mf->package mf)))
          (list (package-name p) p))) mfs)
     (package-inputs cups-filters)))
   (arguments
    (let* ((arguments-alist
            (even-list->alist (package-arguments cups-filters)))
           (phases*
            (append
             (assoc-ref arguments-alist #:phases)
             (map
              (lambda (mf)
                `(add-before
                  'wrap-filters
                  ',(string->symbol
                     (string-append
                      "install-" (symbol->string mf) "-files"))
                  ,(install-symlinks mf))) mfs))))
      (append
       (list
        #:validate-runpath? #f
        #:modules '((ice-9 match)
                    (guix build gnu-build-system)
                    (guix build utils)))
       (alist->even-list
        (alist-replace
         #:phases phases*
         arguments-alist)))))))

(define-public (cups* . mfs)
  (package
   (inherit cups)
   (arguments
    (let* ((arguments-alist
            (even-list->alist (package-arguments cups)))
           (phases*
            (append (assoc-ref arguments-alist #:phases)
                    (list `(replace 'install-cups-filters-symlinks ,install-cups-filters-symlinks))
                    ;; NOTE: (assoc-ref arguments-alist #:phases) seems to produce quoted phases expression from that package. it has to be 'evaluated' in some sence
                    )))
      (append
       (list
        #:validate-runpath? #f
        #:modules '((ice-9 match)
                    (guix build gnu-build-system)
                    (guix build utils)))
       (alist->even-list
        (alist-replace
         #:phases phases*
         arguments-alist)))))
   (inputs
    (modify-inputs
     (package-inputs cups)
     (append system-config-printer)
     (replace "cups-filters"
              (apply cups-filters* mfs))))))
