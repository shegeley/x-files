(define-module (x-files packages darktable-presets)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module ((gnu packages guile)      #:select (guile-3.0 guile-sqlite3 guile-zlib))
  #:use-module ((gnu packages photo)      #:select (darktable))

  #:export (darktable-import-style
            darktable-presets-bovender))

;; A darktable .dtstyle (full style, applied by name via `darktable-cli
;; --style') and .dtpreset (single-module preset, applied only via
;; autoapply metadata matching -- darktable-cli has no per-image
;; name-targeted equivalent for these) both live outside darktable's own
;; sqlite database (data.db, tables `styles'/`style_items'/`presets')
;; until explicitly imported -- dropping the XML file into
;; ~/.config/darktable/styles/ alone does NOT make darktable see it. This
;; script does that import directly, mapping the XML 1:1 onto the DB
;; schema (verified against a real darktable 5.6.0 data.db).

(define darktable-import-style-script
  (program-file
   "darktable-import-style"
   ;; WITH-EXTENSIONS, not WITH-IMPORTED-MODULES: (sqlite3)/(zlib) are FFI
   ;; bindings that dynamic-link libsqlite3/libz at load time --
   ;; WITH-IMPORTED-MODULES re-extracts/recompiles them from source inside
   ;; this derivation's own sandbox, which lacks that link target.
   ;; WITH-EXTENSIONS instead reuses guile-sqlite3/guile-zlib's own
   ;; already-built (and already-linked) module directories as-is, AND --
   ;; unlike a hand-rolled #:module-path, which only wires up %load-path --
   ;; correctly wires up %load-compiled-path too, which program-file's
   ;; `guile --no-auto-compile' shebang requires (verified: a plain
   ;; #:module-path fails at runtime with "no code for module (zlib)").
   (with-extensions (list guile-sqlite3 guile-zlib)
   #~(begin
         (use-modules (sxml simple)
                      (ice-9 match)
                      ((ice-9 regex) #:select (string-match match:substring))
                      ((rnrs bytevectors) #:select (make-bytevector
                                                    bytevector-u8-set!))
                      ((guix base64) #:select (base64-decode))
                      ((zlib) #:select (uncompress))
                      ((sqlite3) #:select (sqlite-open
                                          sqlite-close
                                          sqlite-prepare
                                          sqlite-bind-arguments
                                          sqlite-step
                                          sqlite-finalize
                                          SQLITE_OPEN_READWRITE)))

         (define (usage!)
           (format (current-error-port)
                   "usage: darktable-import-style FILE.dtstyle|FILE.dtpreset [DATA-DB]~%~
                    DATA-DB defaults to $HOME/.config/darktable/data.db~%")
           (exit 1))

         (define (sxml-text tree tag)
           (match (assq tag (cdr tree))
             ((_ text) text)
             ((_) "")
             (#f "")))

         (define (child tree tag)
           (assq tag (cdr tree)))

         (define (hex-decode str)
           ;; darktable's op_params is always plain hex, two hex chars per
           ;; byte, no separators.
           (let* ((len (quotient (string-length str) 2))
                  (bv (make-bytevector len)))
             (let loop ((i 0))
               (when (< i len)
                 (bytevector-u8-set!
                  bv i
                  (string->number (substring str (* i 2) (+ (* i 2) 2)) 16))
                 (loop (+ i 1))))
             bv))

         (define (decode-op-params text)
           ;; op_params never carries the "gz" prefix in any sample seen --
           ;; always plain hex.
           (if (or (not text) (string-null? text)) #vu8() (hex-decode text)))

         (define (decode-blob text)
           ;; blendop_params is either plain hex (older blendop_version) or,
           ;; prefixed "gz<2-digit version>", zlib-compressed data
           ;; base64-encoded on top -- verified against a real darktable
           ;; 5.6.0 .dtstyle export (zlib, not gzip, despite the "gz" name:
           ;; the payload's decoded magic bytes are 0x78 0x9c).
           (if (or (not text) (string-null? text))
               #vu8()
               (let ((m (string-match "^gz([0-9][0-9])(.*)$" text)))
                 (if m
                     (uncompress (base64-decode (match:substring m 2)))
                     (hex-decode text)))))

         (define (run! db stmt . args)
           (let ((s (sqlite-prepare db stmt)))
             (apply sqlite-bind-arguments s args)
             (sqlite-step s)
             (sqlite-finalize s)))

         (define (import-style! db info style)
           (let* ((name (sxml-text info 'name))
                  (description (sxml-text info 'description))
                  (iop-list (sxml-text info 'iop_list)))
             (run! db "DELETE FROM style_items WHERE styleid IN (SELECT id FROM styles WHERE name = ?)" name)
             (run! db "DELETE FROM styles WHERE name = ?" name)
             (run! db "INSERT INTO styles (name, description, iop_list) VALUES (?, ?, ?)"
                   name description iop-list)
             (let* ((s (sqlite-prepare db "SELECT last_insert_rowid()"))
                    (_ (sqlite-step s))
                    (styleid (vector-ref (sqlite-step s) 0)))
               (sqlite-finalize s)
               (for-each
                (lambda (plugin)
                  (run! db
                        "INSERT INTO style_items
                         (styleid, num, module, operation, op_params, enabled,
                          blendop_params, blendop_version, multi_priority,
                          multi_name, multi_name_hand_edited)
                         VALUES (?,?,?,?,?,?,?,?,?,?,?)"
                        styleid
                        (string->number (sxml-text plugin 'num))
                        (string->number (sxml-text plugin 'module))
                        (sxml-text plugin 'operation)
                        (decode-op-params (sxml-text plugin 'op_params))
                        (string->number (sxml-text plugin 'enabled))
                        (decode-blob (sxml-text plugin 'blendop_params))
                        (string->number (sxml-text plugin 'blendop_version))
                        (string->number (sxml-text plugin 'multi_priority))
                        (sxml-text plugin 'multi_name)
                        (string->number (or (sxml-text plugin 'multi_name_hand_edited) "0"))))
                (filter (lambda (x) (and (pair? x) (eq? (car x) 'plugin))) (cdr style))))
             (format #t "imported style ~s into ~a~%" name db)))

         (define (import-preset! db preset)
           (let ((name (sxml-text preset 'name))
                 (description (sxml-text preset 'description))
                 (operation (sxml-text preset 'operation)))
             (run! db "DELETE FROM presets WHERE name = ? AND operation = ?" name operation)
             (run! db
                   "INSERT INTO presets
                    (name, description, operation, op_version, op_params, enabled,
                     blendop_params, blendop_version, multi_priority, multi_name,
                     multi_name_hand_edited, model, maker, lens, iso_min, iso_max,
                     exposure_min, exposure_max, aperture_min, aperture_max,
                     focal_length_min, focal_length_max, writeprotect, autoapply,
                     filter, def, format)
                    VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
                   name description operation
                   (string->number (or (sxml-text preset 'op_version) "0"))
                   (decode-op-params (sxml-text preset 'op_params))
                   (string->number (or (sxml-text preset 'enabled) "1"))
                   (decode-blob (sxml-text preset 'blendop_params))
                   (string->number (or (sxml-text preset 'blendop_version) "0"))
                   (string->number (or (sxml-text preset 'multi_priority) "0"))
                   (sxml-text preset 'multi_name)
                   (string->number (or (sxml-text preset 'multi_name_hand_edited) "0"))
                   ;; wildcard match conditions: autoapply on every shot,
                   ;; not tied to a specific camera/lens/exposure -- this
                   ;; is the closest a .dtpreset gets to "apply by name":
                   ;; darktable-cli --apply-custom-presets 1 picks it up
                   ;; on export, unconditionally.
                   "%" "%" "%" 0.0 51200000.0 -8.0 8.0 0.0 100000.0 0.0 1000.0
                   1 1 0 0 0)
             (format #t "imported preset ~s (module ~a) into ~a, autoapply/wildcard~%"
                     name operation db)))

         (let* ((args (cdr (program-arguments)))
                (file (if (>= (length args) 1) (car args) (usage!)))
                (db-path (if (>= (length args) 2)
                             (cadr args)
                             (string-append (getenv "HOME") "/.config/darktable/data.db")))
                (tree (call-with-input-file file xml->sxml))
                (root (car tree)))
           (unless (file-exists? db-path)
             (format (current-error-port) "no such database: ~a~%" db-path)
             (exit 1))
           (let ((db (sqlite-open db-path SQLITE_OPEN_READWRITE)))
             (match root
               ('darktable_style
                (let ((info (child tree 'info))
                      (style (child tree 'style)))
                  (import-style! db info style)))
               ('darktable_preset
                (import-preset! db (child tree 'preset)))
               (_
                (format (current-error-port) "unrecognized root element: ~a~%" root)
                (exit 1)))
             (sqlite-close db)))))))

(define-public darktable-import-style-package
  (package
    (name "darktable-import-style")
    (version "0.0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p (string-append #$output "/bin"))
          (symlink #$darktable-import-style-script
                   (string-append #$output "/bin/darktable-import-style")))))
    (propagated-inputs (list guile-3.0 guile-sqlite3 guile-zlib))
    (synopsis "Import darktable .dtstyle/.dtpreset files by direct SQL insert")
    (description "Imports a darktable style (@code{.dtstyle}) or module
preset (@code{.dtpreset}) XML file straight into darktable's own
@file{data.db}, the same tables the GUI's Import button writes to.
Styles become immediately usable via @code{darktable-cli --style NAME};
presets become autoapply (wildcard camera/lens/exposure match), picked
up by @code{darktable-cli --apply-custom-presets 1} on export.")
    (home-page "https://www.darktable.org")
    (license license:gpl3+)))

(define %bovender-commit "aada108b1b7b0299bebfb5e8a8a81b299833d271")

(define-public darktable-presets-bovender
  (package
    (name "darktable-presets-bovender")
    (version (git-version "0.0.0" "1" %bovender-commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bovender/darktable-presets-and-styles")
             (commit %bovender-commit)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1hd0h2nkadwb8c3az7qi32978as6sif9iz28qqzqaslndpy0315v"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("." "share/darktable-presets-bovender"
           #:include-regexp ("\\.dtpreset$" "\\.dtstyle$" "^README" "^LICENSE")))))
    (synopsis "Bovender's CC0 darktable module presets")
    (description "A small collection of darktable module presets
(@code{.dtpreset}), public domain (CC0), from
@url{https://github.com/bovender/darktable-presets-and-styles}.  Import
with @code{darktable-import-style} from @code{darktable-import-style}
(this channel), then run @code{darktable-cli ... --apply-custom-presets 1}
to have them picked up automatically on export.")
    (home-page "https://github.com/bovender/darktable-presets-and-styles")
    (license license:cc0)))
