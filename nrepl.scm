(use-modules
 (guix gexp) ;; has to be required or nrepl (arei/ares) won't recognize gexp syntax #~, #$
 (nrepl server))

(run-nrepl-server #:port 7888)
