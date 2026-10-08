;;; split-mdbook.scm --- Split an mdBook print.html into per-heading pages.
;;;
;;; mdBook's print.html concatenates the whole book into one multi-megabyte
;;; page.  EWW/shr renders such pages pathologically slowly (tens of seconds
;;; with Emacs blocked), so the packaged manuals are split at heading
;;; boundaries into small pages instead.  Intra-book links — bare "#anchor"
;;; hrefs and same-site absolute paths like "/manual/nixpkgs/unstable/#anchor",
;;; single- or double-quoted — are rewritten to point at the page containing
;;; their target, and an index.html table of contents in document order is
;;; generated.
;;;
;;; All scanning is done with string-contains over literal prefixes: Guile's
;;; regexp engine is far too slow on multi-megabyte strings.  Loaded with
;;; (load ...) from the nix-manuals builder and from tests; depends only on
;;; Guile core.

(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-13))

(define (split-mdbook/read-file path)
  (call-with-port (open-input-file path #:encoding "UTF-8")
    get-string-all))

(define (split-mdbook/write-file path text)
  (call-with-port (open-output-file path #:encoding "UTF-8")
    (lambda (port) (display text port))))

(define (split-mdbook/find-all text needle)
  "Return all positions where the literal NEEDLE occurs in TEXT."
  (let ((width (string-length needle)))
    (let loop ((pos 0) (acc '()))
      (let ((index (string-contains text needle pos)))
        (if index
            (loop (+ index width) (cons index acc))
            (reverse acc))))))

(define (split-mdbook/quoted-value text position quote)
  "Read the attribute value starting at POSITION (just past the opening
QUOTE character).  Return (value . position-after-closing-quote)."
  (let ((close (string-index text quote position)))
    (unless close
      (error "Unterminated attribute value" position))
    (cons (substring text position close) (1+ close))))

(define (split-mdbook/attribute-value text position)
  "Read the \"...\" attribute value starting at POSITION (just past the
opening quote of the value).  Return (value . position-after-closing-quote)."
  (split-mdbook/quoted-value text position #\"))

(define (split-mdbook/replace-all text from to)
  "Replace every occurrence of the literal FROM in TEXT with TO."
  (let ((width (string-length from)))
    (call-with-output-string
     (lambda (out)
       (let loop ((pos 0))
         (let ((index (string-contains text from pos)))
           (if index
               (begin
                 (display (substring text pos index) out)
                 (display to out)
                 (loop (+ index width)))
               (display (substring text pos) out))))))))

(define (split-mdbook/strip-tags text)
  (call-with-output-string
   (lambda (out)
     (let loop ((pos 0))
       (let ((open (string-index text #\< pos)))
         (if (not open)
             (display (substring text pos) out)
             (let ((close (string-index text #\> open)))
               (display (substring text pos open) out)
               (loop (if close (1+ close) (string-length text))))))))))

(define (split-mdbook/decode-entities text)
  (fold (lambda (pair acc)
          (split-mdbook/replace-all acc (car pair) (cdr pair)))
        text
        '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
          ("&quot;" . "\"") ("&#39;" . "'"))))

(define (split-mdbook/encode-entities text)
  (split-mdbook/replace-all
   (split-mdbook/replace-all
    (split-mdbook/replace-all text "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

(define (split-mdbook/mkdir-p dir)
  (unless (file-exists? dir)
    (split-mdbook/mkdir-p (dirname dir))
    (mkdir dir)))

(define (split-mdbook/headings text max-level)
  "Return (position level id) triples for headings of levels
1..MAX-LEVEL carrying an id, in document order."
  (let ((found
         (append-map
          (lambda (level)
            (let* ((prefix (string-append "<h" (number->string level)
                                          " id=\""))
                   (width (string-length prefix)))
              (map (lambda (position)
                     (let ((value (split-mdbook/attribute-value
                                   text (+ position width))))
                       (list position level (car value))))
                   (split-mdbook/find-all text prefix))))
          (iota max-level 1))))
    (sort found (lambda (a b) (< (car a) (car b))))))

(define (split-mdbook/check-page-name id)
  (unless (and (not (string-null? id))
               (not (string=? id "index"))
               (string-every
                (lambda (c)
                  (or (char-alphabetic? c)
                      (char-numeric? c)
                      (memv c '(#\- #\_ #\.))))
                id))
    (error "Heading id is not a safe page name" id)))

(define (split-mdbook/sections text headings)
  "Split TEXT at HEADINGS into (level id title content) sections."
  (map
   (lambda (heading next)
     (match-let (((position level id) heading))
       (split-mdbook/check-page-name id)
       (let* ((tag-end (string-index text #\> position))
              (close (string-contains text "</h" tag-end)))
         (list level id
               (split-mdbook/decode-entities
                (split-mdbook/strip-tags
                 (substring text (1+ tag-end) close)))
               (substring text position
                          (if next (car next) (string-length text)))))))
   headings
   (append (cdr headings) '(#f))))

(define (split-mdbook/index-anchors sections)
  "Map every id anchor in SECTIONS, single- or double-quoted, to the page
file of its section."
  (let ((id->page (make-hash-table)))
    (for-each
     (lambda (section)
       (match-let (((_level id _title content) section))
         (let loop ((pos 0))
           (let ((attr (split-mdbook/next-attribute content pos "id")))
             (when attr
               (let ((value (split-mdbook/quoted-value
                             content (+ (car attr) (string-length "id=") 1)
                             (cdr attr))))
                 (unless (hash-ref id->page (car value))
                   (hash-set! id->page (car value)
                              (string-append id ".html")))
                 (loop (cdr value))))))))
     sections)
    id->page))

(define (split-mdbook/next-attribute text position name)
  "Find the next NAME attribute at or after POSITION.  Return (index . quote)
where QUOTE is the attribute's quote character, or #f."
  (let ((double (string-contains text (string-append name "=\"") position))
        (single (string-contains text (string-append name "='") position)))
    (cond ((and double single) (if (< double single)
                                   (cons double #\")
                                   (cons single #\')))
          (double (cons double #\"))
          (single (cons single #\'))
          (else #f))))

(define (split-mdbook/local-fragment href)
  "Return the fragment of HREF when it refers to this book: bare \"#anchor\"
links and same-site absolute paths like \"/manual/nixpkgs/unstable/#anchor\".
Return #f for external links and for paths without a fragment."
  (cond ((string-prefix? "#" href) (substring href 1))
        ((string-prefix? "/" href)
         (let ((hash (string-index href #\#)))
           (and hash (substring href (1+ hash)))))
        (else #f)))

(define (split-mdbook/rewrite-hrefs content id->page current-page)
  "Rewrite same-book hrefs — bare \"#anchor\" links and same-site absolute
paths, single- or double-quoted — to the page containing their anchor.
Same-page anchors become bare \"#anchor\" links; unknown-anchor and external
hrefs are kept untouched."
  (call-with-output-string
   (lambda (out)
     (let loop ((pos 0))
       (let ((href (split-mdbook/next-attribute content pos "href")))
         (if (not href)
             (display (substring content pos) out)
             (let* ((start (car href))
                    (quote (cdr href))
                    (value (split-mdbook/quoted-value
                            content (+ start (string-length "href=") 1) quote))
                    (fragment (split-mdbook/local-fragment (car value)))
                    (page (and fragment (hash-ref id->page fragment))))
               (display (substring content pos start) out)
               (if page
                   (if (string=? page current-page)
                       (format out "href=~c#~a~c" quote fragment quote)
                       (format out "href=~c~a#~a~c" quote page fragment quote))
                   (display (substring content start (cdr value)) out))
               (loop (cdr value)))))))))

(define (split-mdbook/page title content)
  (string-append
   "<!DOCTYPE html>\n<html><head><meta charset=\"utf-8\"><title>"
   (split-mdbook/encode-entities title)
   "</title></head><body>\n"
   content
   "\n</body></html>\n"))

(define* (split-mdbook-print input-file output-directory max-level
                             #:key (book-title "Contents"))
  "Split mdBook print.html INPUT-FILE into one page per heading of levels
1..MAX-LEVEL carrying an id, writing the pages and a generated index.html
table of contents into OUTPUT-DIRECTORY.  Intra-book links (bare \"#anchor\"
hrefs and same-site absolute paths) are rewritten to the page containing
their target.  Return the page count."
  (let* ((text (split-mdbook/read-file input-file))
         (sections (split-mdbook/sections
                    text (split-mdbook/headings text max-level)))
         (id->page (split-mdbook/index-anchors sections)))
    (when (null? sections)
      (error "No headings with ids found; not an mdBook print.html?"
             input-file))
    (let ((seen '()))
      (for-each (lambda (section)
                  (when (member (cadr section) seen)
                    (error "Duplicate heading id" (cadr section)))
                  (set! seen (cons (cadr section) seen)))
                sections))
    (split-mdbook/mkdir-p output-directory)
    (for-each
     (lambda (section)
       (match-let (((_level id title content) section))
         (split-mdbook/write-file
          (string-append output-directory "/" id ".html")
          (split-mdbook/page
           title
           (split-mdbook/rewrite-hrefs content id->page
                                       (string-append id ".html"))))))
     sections)
    (split-mdbook/write-file
     (string-append output-directory "/index.html")
     (split-mdbook/page
      book-title
      (call-with-output-string
       (lambda (out)
         (format out "<h1>~a</h1>\n" (split-mdbook/encode-entities book-title))
         (let loop ((rest sections) (depth 0))
           (match rest
             (()
              (for-each (lambda (_) (display "</ul>\n" out)) (iota depth)))
             (((level id title _content) . more)
              (cond
               ((> level depth)
                (for-each (lambda (_) (display "<ul>\n" out))
                          (iota (- level depth))))
               ((< level depth)
                (for-each (lambda (_) (display "</ul>\n" out))
                          (iota (- depth level)))))
              (format out "<li><a href=\"~a.html\">~a</a></li>\n"
                      id (split-mdbook/encode-entities title))
              (loop more level))))))))
    (length sections)))
