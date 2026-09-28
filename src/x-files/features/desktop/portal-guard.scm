(define-module (x-files features desktop portal-guard)
  #:use-module ((x-files utils recovery) #:select (recover-in-order!))
  #:export (repair-screen-cast-portal!))

(define (positive-integer? value)
  (and (integer? value) (positive? value)))

(define* (repair-screen-cast-portal!
          #:key
          shell-ready?
          backend-source-types
          portal-source-types
          restart-backend!
          restart-portal!
          (pause sleep)
          (shell-attempts 60)
          (backend-attempts 15)
          (portal-attempts 5)
          (delay 1))
  "Repair the GNOME ScreenCast portal in dependency order.

Wait for @var{shell-ready?}, then require positive masks from
@var{backend-source-types} and @var{portal-source-types}, in that order.
The probes and @var{restart-backend!}/@var{restart-portal!} callbacks take no
arguments.  Restart each unhealthy owner at most once, then probe again.

@var{shell-attempts}, @var{backend-attempts}, and @var{portal-attempts} bound
the probe count in each waiting phase; @var{pause} receives @var{delay}
between failed probes.  Return the @code{recover-in-order!} result with
@code{shell}, @code{backend}, and @code{portal} step names."
  (recover-in-order!
   `(((name . shell)
      (probe . ,shell-ready?)
      (healthy? . ,(lambda (value) value))
      (attempts . ,shell-attempts))
     ((name . backend)
      (probe . ,backend-source-types)
      (healthy? . ,positive-integer?)
      (repair! . ,restart-backend!)
      (attempts . ,backend-attempts))
     ((name . portal)
      (probe . ,portal-source-types)
      (healthy? . ,positive-integer?)
      (repair! . ,restart-portal!)
      (attempts . ,portal-attempts)))
   #:delay delay
   #:pause pause))
