(use-modules ((ice-9 match) #:select (match))
             ((srfi srfi-13) #:select (string-prefix?))
             ((sxml simple) #:select (sxml->xml))
             ((system foreign) #:select (%null-pointer int unsigned-int void
                                                       pointer->procedure procedure->pointer
                                                       string->pointer make-c-struct)))

(define (serve-fixture library role scenario directory)
  "Serve @var{role} on the private bus under @var{directory}.
Bind the required GIO calls directly from @var{library}.  Each process
increments its role's counter and exposes the source mask selected by
@var{scenario}: @code{0} while stale, @code{7} once recovered.  The
@code{shell} role only owns its bus name."
  (unless (string-prefix? (string-append "unix:path=" directory "/bus")
                          (or (getenv "DBUS_SESSION_BUS_ADDRESS") ""))
    (error "Fixture requires its own private bus"))
  (let* ((gio (dynamic-link library))
         (bind (lambda (name result arguments)
                 (pointer->procedure result (dynamic-func name gio) arguments)))
         (own-name (bind "g_bus_own_name" unsigned-int
                         (list int '* unsigned-int '* '* '* '* '*)))
         (node-info (bind "g_dbus_node_info_new_for_xml" '* (list '* '*)))
         (lookup (bind "g_dbus_node_info_lookup_interface" '* (list '* '*)))
         (register (bind "g_dbus_connection_register_object" unsigned-int
                         (list '* '* '* '* '* '* '*)))
         (variant (bind "g_variant_new_uint32" '* (list unsigned-int)))
         (new-loop (bind "g_main_loop_new" '* (list '* int)))
         (run-loop (bind "g_main_loop_run" void (list '*)))
         (counter (string-append directory "/" role))
         (count (if (file-exists? counter)
                    (1+ (call-with-input-file counter read)) 1))
         (stale? (member role (assoc-ref
                               '(("healthy") ("frontend" "portal")
                                 ("both" "backend" "portal")
                                 ("backend-failure" "backend")
                                 ("frontend-failure" "portal")) scenario)))
         (mask (if (and stale?
                        (or (= count 1)
                            (member scenario '("backend-failure" "frontend-failure"))))
                   0 7))
         (name (assoc-ref '(("shell" . "org.gnome.Shell")
                            ("backend" . "org.freedesktop.impl.portal.desktop.gnome")
                            ("portal" . "org.freedesktop.portal.Desktop")) role))
         (interface (string-append "org.freedesktop."
                                   (if (equal? role "backend") "impl." "")
                                   "portal.ScreenCast"))
         (info (node-info
                (string->pointer
                 (with-output-to-string
                   (lambda ()
                     (sxml->xml
                      `(node (interface (@ (name ,interface))
                                        (property (@ (name "AvailableSourceTypes")
                                                     (type "u") (access "read")))))))))
                %null-pointer))
         (getter (procedure->pointer '* (lambda _ (variant mask))
                                     (make-list 7 '*)))
         (vtable (make-c-struct (make-list 11 '*)
                                (append (list %null-pointer getter %null-pointer)
                                        (make-list 8 %null-pointer))))
         (acquired
          (procedure->pointer
           void
           (lambda (connection _name _data)
             (unless (equal? role "shell")
               (when (zero? (register connection
                                      (string->pointer "/org/freedesktop/portal/desktop")
                                      (lookup info (string->pointer interface))
                                      vtable %null-pointer %null-pointer %null-pointer))
                 (error "Could not register test property"))))
           (list '* '* '*))))
    (call-with-output-file counter (lambda (port) (write count port)))
    (own-name 2 (string->pointer name) 0 acquired
              %null-pointer %null-pointer %null-pointer %null-pointer)
    (run-loop (new-loop %null-pointer 0))))

(match (command-line)
  ((_ library role scenario directory)
   (serve-fixture library role scenario directory)))
