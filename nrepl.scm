(use-modules
 (guix gexp) ;; has to be required or nrepl (arei/ares) won't recognize gexp syntax #~, #$
 (ares server))

(run-nrepl-server #:port 7888)
