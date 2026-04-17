(define-module (x-files services spotify)
  #:use-module (guix gexp)
  #:use-module ((gnu services)       #:select (service-extension
                                               service-type))
  #:use-module ((gnu home services)  #:select (home-profile-service-type
                                               home-files-service-type
                                               home-activation-service-type))
  #:use-module ((x-files packages spotify) #:select (spotify))
  #:export (spotify-home-service-type
            spotify-default-config))

;; Config is a plain alist.  Keys:
;;   package    — spotify package object
;;   data-dir   — passed as --datapath to the binary;
;;                #f = default (~/.config/spotify)
;;   cache-size — local cache limit in MB written to prefs on first run;
;;                #f = Spotify's own default (10 240 MB)
;;   extra-args — list of extra CLI argument strings, default '()

(define spotify-default-config
  `((package    . ,spotify)
    (data-dir   . #f)
    (cache-size . #f)
    (extra-args . ())))

(define (cfg c k) (assoc-ref c k))

(define (spotify-launcher config)
  "Return a computed-file for a shell wrapper that applies configured flags."
  (let* ((pkg      (cfg config 'package))
         (data-dir (cfg config 'data-dir))
         (extra    (cfg config 'extra-args))
         (args     (append
                    (if data-dir
                        (list (string-append "--datapath=" data-dir))
                        '())
                    extra)))
    (computed-file "spotify"
      #~(begin
          (call-with-output-file #$output
            (lambda (port)
              (format port "#!/bin/sh\nexec ~a ~a \"$@\"\n"
                      #$(file-append pkg "/bin/spotify")
                      #$(string-join args " "))))
          (chmod #$output #o755)))))

(define (spotify-need-launcher? config)
  (or (cfg config 'data-dir)
      (not (null? (cfg config 'extra-args)))))

(define (spotify-home-files config)
  "If flags are set, shadow the profile wrapper with a configured launcher."
  (if (spotify-need-launcher? config)
      `((".local/bin/spotify" ,(spotify-launcher config)))
      '()))

(define (spotify-activation config)
  "Create the data directory and write initial prefs when cache-size is set."
  (let ((data-dir   (cfg config 'data-dir))
        (cache-size (cfg config 'cache-size)))
    #~(begin
        (use-modules (guix build utils))
        ;; Resolve the effective config directory at activation time.
        (let* ((home     (getenv "HOME"))
               (conf-dir (or #$data-dir
                             (string-append home "/.config/spotify"))))
          (mkdir-p conf-dir)
          ;; Write prefs only if cache-size is configured and prefs don't exist yet.
          (when #$(and cache-size #t)
            (let ((prefs (string-append conf-dir "/prefs")))
              (unless (file-exists? prefs)
                (call-with-output-file prefs
                  (lambda (port)
                    (display #$(string-append "storage.size="
                                              (number->string cache-size)
                                              "\n")
                             port))))))))))

(define-public spotify-home-service-type
  (service-type
   (name 'spotify)
   (description
    "Configure the Spotify music client: install the package, optionally
redirect its data directory, cap local cache size, and pass extra flags.")
   (extensions
    (list
     (service-extension home-profile-service-type
                        (lambda (config) (list (cfg config 'package))))
     (service-extension home-files-service-type
                        spotify-home-files)
     (service-extension home-activation-service-type
                        spotify-activation)))
   (default-value spotify-default-config)))
