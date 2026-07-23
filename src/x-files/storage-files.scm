(define-module (x-files storage-files)
  #:use-module ((guix gexp) #:select (local-file))

  #:export (browser-history-manager-scrub))

;; Named file-likes for non-module resources bundled in the channel's storage/
;; directory (kept OFF the src/ module path so `guix pull' does not try to
;; compile them).
;;
;; Cuirass-safety: `local-file' resolves a LITERAL path against THIS module's
;; source directory at macro-expansion (see (guix gexp) `local-file'); a
;; computed/non-literal path would instead be resolved against `getcwd' and
;; break under Cuirass / a pulled channel.  So each entry is a literal relative
;; `local-file' — correct from a local checkout, a pulled channel, and a
;; Cuirass evaluation alike.  The relative path lives HERE only, so service and
;; test modules just import the named file-like.

(define browser-history-manager-scrub
  (local-file "../../storage/scripts/browser-history-manager.scm"
              "browser-history-manager.scm"))
