(define-module (x-files features grafana)
  #:use-module (rde features)
  #:use-module (rde predicates)
  #:use-module (gnu services)
  #:use-module ((x-files packages grafana) #:select (grafana-bin))
  #:use-module ((x-files services grafana) #:select (grafana-service-type
                                                     grafana-default-config))
  #:export (feature-grafana))

(define* (feature-grafana
          #:key
          (grafana-package grafana-bin)
          (http-port "3000")
          (http-addr "")
          (data-dir  "/var/lib/grafana")
          (log-dir   "/var/log/grafana")
          (cfg-path  #f))
  "Run Grafana observability platform as a native shepherd service."
  (ensure-pred string? http-port)
  (ensure-pred string? http-addr)
  (ensure-pred string? data-dir)
  (ensure-pred string? log-dir)

  (define grafana-config
    `((package  . ,grafana-package)
      (port     . ,http-port)
      (addr     . ,http-addr)
      (data-dir . ,data-dir)
      (log-dir  . ,log-dir)
      (cfg-path . ,cfg-path)))

  (define (get-system-services _)
    (list (service grafana-service-type grafana-config)))

  (feature
   (name 'grafana)
   (values `((grafana . #t)))
   (system-services-getter get-system-services)))
