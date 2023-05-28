(define-module (x-files services direnv)
  #:use-module (ice-9 match)
  #:use-module (gnu services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services shells)
  #:use-module (guix records)
  #:use-module (guix gexp)

  #:use-module (gnu home services)

  #:use-module (gnu home-services shellutils)

  #:export (bash-direnv-service-type))

;; (define-record-type* <direnv-conf>
;;   direnv-conf
;;   direnv-conf!
;;   direnv-conf~
;;   direnv-conf?
;;   (config direnv-conf:config)
;;   (global direnv-conf:global)
;;   (whitelist direnv-conf:whitelist))


;; (define config
;;   `((section . "global") .
;;     (entries . (("load_dotenv" . #t)
;;                 ("disable_stdin" . #t)))))


;; (define (->toml entries)
;;   ;; entries := (section1 . ((k1 . v1)
;;   ;;                         (k2 . v2) ...)
;;   ;;             section2 . ((k1 . v1) ...)
;;   ;; when section is #t it's treated as 'global'
;;   (let* ((serialize
;;           (lambda (x)
;;             (match x
;;               (#t "true")
;;               (#f "false")
;;               (_ x)))))
;;     (match entries
;;       ((sec . rest)
;;        (string-append "[" sec "]"
;;                       (->toml rest)))
;;       (((k . v) rest ...)
;;        (string-append
;;         k " = " (serialize v) "\n"
;;         (->toml rest)))
;;       ('() ""))))

;; NOTE: Use simplest possible decision for now. Refactor all above this note later

(define config*
  (string-append
   "[global]" "\n"
   "load_dotenv = true"))

(define (files config)
  `((".config/direnv/direnv.toml"
     ,(plain-file "direnv.toml" config))))

(define bash-direnv-service-type
  (service-type
   (inherit home-bash-direnv-service-type)
   (default-value config*)
   (extensions
    (append
     (list (service-extension home-files-service-type files))
     (service-type-extensions home-bash-direnv-service-type)))))
