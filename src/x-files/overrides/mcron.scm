(define-module (x-files overrides mcron)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services ssh)

  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)

  #:use-module (guix records)
  #:use-module (guix gexp)

  #:use-module (gnu packages guile-xyz)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)

  #:export (home-mcron-service-type))

(define (home-mcron-shepherd-services config)
  (match-record
   config (@@ (gnu home services mcron) <home-mcron-configuration>)
   (mcron jobs log? log-format)
   (if (null? jobs)
       '()                                       ;no jobs to run
       (let ((files ((@@ (gnu home services mcron) job-files) mcron jobs)))
         (list (shepherd-service
                (documentation "User cron jobs.")
                (provision '(mcron))
                (modules `((srfi srfi-1)
                           (srfi srfi-26)
                           (ice-9 popen)         ;for the 'schedule' action
                           (ice-9 rdelim)
                           (ice-9 match)
                           ,@%default-modules))
                (start #~(make-forkexec-constructor
                          (list (string-append #$mcron "/bin/mcron")
                                #$@(if log?
                                       #~("--log" "--log-format" #$log-format)
                                       #~())
                                #$@files)
                          #:log-file (string-append
                                      (or (getenv "XDG_STATE_HOME")
                                          (format #f "~a/.local/state"
                                                  (getenv "HOME")))
                                      "/log/mcron.log")))
                (stop #~(make-kill-destructor))
                (actions
                 (list ((@@ (gnu home services mcron) shepherd-schedule-action) mcron files)))))))))

(define home-mcron-service-type
  (service-type
   (name 'home-mcron)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           home-mcron-shepherd-services)
          (service-extension
           home-profile-service-type
           (@@ (gnu home services mcron) home-mcron-profile))))
   (compose concatenate)
   (extend (@@ (gnu home services mcron) home-mcron-extend))
   (default-value ((@@ (gnu home services mcron) home-mcron-configuration)))
   (description
    "Install and configure the GNU mcron cron job manager.")))
