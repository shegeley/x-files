(define-module (x-files packages firefox)
  #:use-module (gnu)

  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)

  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)

  #:use-module (nonguix build-system binary)
  #:use-module (guix build-system trivial)

  #:use-module (nongnu packages mozilla)

  #:export (firefox/wayland-
            firefox*))

(define firefox*
  (package/inherit
   firefox
    (inputs
    (modify-inputs
     (package-inputs firefox)
     (delete "pipewire")
     (append pipewire)))))

(define firefox/wayland-
  (package
    (inherit firefox*)
    (name "firefox-wayland-")
    (native-inputs '())
    (inputs
     `(("bash" ,bash-minimal)
       ("pipewire" ,pipewire)
       ("firefox" ,firefox*)))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((bash    (assoc-ref %build-inputs "bash"))
                (firefox (assoc-ref %build-inputs "firefox"))
                (pipewire (assoc-ref %build-inputs "pipewire"))
                (out     (assoc-ref %outputs "out"))
                (exe     (string-append out "/bin/firefox")))
           (mkdir-p (dirname exe))

           (call-with-output-file exe
             (lambda (port)
               ;; NOTE: added "export LD_LIBRARY_PATH=pipewire"
               ;; maybe this can be done better with `wrap-programm'
               (format port "#!~a \n
export LD_LIBRARY_PATH=~a \n
export MOZ_ENABLE_WAYLAND=1 \n
exec ~a $@\n"
                       (string-append bash "/bin/bash")
                       (string-append pipewire "/lib")
                       (string-append firefox "/bin/firefox"))))
           (chmod exe #o555)

           ;; Provide the manual and .desktop file.
           (copy-recursively (string-append firefox "/share")
                             (string-append out "/share"))
           (substitute* (string-append
                         out "/share/applications/firefox.desktop")
             ((firefox) out))
           #t))))))
