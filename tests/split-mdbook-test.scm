(use-modules ((ares suitbl) #:select (suite test is current-test-runner get-state make-suitbl))
             ((ares suitbl state) #:select (get-run-history))
             ((ice-9 textual-ports) #:select (get-string-all))
             ((srfi srfi-1) #:select (every)))

(load (string-append (getcwd) "/src/x-files/packages/aux/nix-manuals/split-mdbook.scm.tmpl"))

(define %fixture
  (string-append
   "<!DOCTYPE html>\n"
   "<html><head><title>chrome</title>"
   "<script>var x = \"<h1 id=\\\"not-a-heading\\\">\";</script></head>\n"
   "<body><nav>chrome</nav>\n"
   "<h1 id=\"chap-one\">Chapter &amp; One</h1>\n"
   "<p>See <a href=\"#sec-two\">section two</a>, "
   "<a href=\"#anchor-in-chap-one\">anchor</a> "
   "and <a href=\"#missing\">missing</a>.</p>\n"
   "<p>DocBook-style: <a href='/manual/nixpkgs/unstable/#sec-two'>absolute</a>, "
   "<a href='#sec-two'>single-quoted</a>, "
   "<a href='/manual/nixpkgs/unstable/#anchor-in-chap-one'>same-page absolute</a>, "
   "<a href='/manual/nixpkgs/unstable/#footnote-x.__back.0'>footnote</a>, "
   "<a href=\"https://example.com/#sec-two\">external</a> "
   "and <a href='/manual/nixpkgs/unstable/release-notes'>fragment-less</a>.</p>\n"
   "<p id=\"anchor-in-chap-one\">text</p>\n"
   "<h2 id=\"local\">Local section</h2>\n"
   "<h1 id=\"chap-two\">Chapter Two</h1>\n"
   "<h2 id=\"sec-two\">Section Two</h2>\n"
   "<p>Back to <a href=\"#chap-one\">one</a> "
   "and <a href=\"#anchor-in-chap-one\">anchor</a>.</p>\n"
   "<p id='footnote-x.__back.0'>note</p>\n"
   "</body></html>\n"))

(define (read-file-string path)
  (call-with-port (open-input-file path #:encoding "UTF-8")
    get-string-all))

(define runner (make-suitbl))
(current-test-runner runner)

(suite "split-mdbook-print"
  (test "Splits pages, rewrites cross-page anchors, generates a TOC" ()
    (let* ((directory (string-append "/tmp/split-mdbook-test-"
                                     (number->string (getpid))))
           (input (string-append directory "/print.html"))
           (output (string-append directory "/out")))
      (split-mdbook/mkdir-p directory)
      (call-with-port (open-output-file input #:encoding "UTF-8")
        (lambda (port) (display %fixture port)))
      (is (= 4 (split-mdbook-print input output 2 #:book-title "Book")))
      (let ((chap-one (read-file-string (string-append output "/chap-one.html")))
            (sec-two (read-file-string (string-append output "/sec-two.html")))
            (toc (read-file-string (string-append output "/index.html"))))
        ;; Cross-page anchor rewritten to the page holding the target.
        (is (string-contains chap-one "href=\"sec-two.html#sec-two\""))
        (is (string-contains sec-two "href=\"chap-one.html#chap-one\""))
        ;; Non-heading anchors are mapped to their page too.
        (is (string-contains sec-two "href=\"chap-one.html#anchor-in-chap-one\""))
        ;; Same-page and unknown anchors are left untouched.
        (is (string-contains chap-one "href=\"#anchor-in-chap-one\""))
        (is (string-contains chap-one "href=\"#missing\""))
        ;; Single-quoted and same-site absolute links are rewritten too,
        ;; preserving their quote style.
        (is (string-contains chap-one "href='sec-two.html#sec-two'"))
        ;; Same-page absolute links are normalized to bare anchors.
        (is (string-contains chap-one "href='#anchor-in-chap-one'"))
        ;; Single-quoted ids are indexed as anchor targets too.
        (is (string-contains chap-one "href='sec-two.html#footnote-x.__back.0'"))
        ;; External links and fragment-less site paths are left untouched.
        (is (string-contains chap-one "href=\"https://example.com/#sec-two\""))
        (is (string-contains chap-one "href='/manual/nixpkgs/unstable/release-notes'"))
        ;; Entities decoded in titles, encoded back in markup.
        (is (string-contains chap-one "<title>Chapter &amp; One</title>"))
        ;; TOC lists every page in document order.
        (is (string-contains toc "<h1>Book</h1>"))
        (is (string-contains toc "href=\"chap-one.html\""))
        (is (string-contains toc "href=\"sec-two.html\""))
        ;; Heading-looking text inside <script> does not split pages.
        (is (not (file-exists? (string-append output "/not-a-heading.html"))))))))

(let ((history (get-run-history (get-state runner))))
  (exit (if (and (pair? history)
                 (every (lambda (run) (eq? 'pass (assq-ref run 'test-run/outcome))) history))
            0 1)))
