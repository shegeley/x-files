(define-module (x-files build podman-storage)
  #:use-module ((guix build syscalls) #:select (mount-points mount umount MS_BIND))
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module ((ice-9 match) #:select (match-lambda))
  #:use-module ((srfi srfi-1) #:select (every))
  #:export (mount-podman-storage unmount-podman-storage))

(define (bindings directory users)
  (map (lambda (user)
         (let ((account (getpwnam user)))
           (list (string-append directory "/podman/" user "/storage")
                 (if (zero? (passwd:uid account))
                     "/var/lib/containers/storage"
                     (string-append (passwd:dir account)
                                    "/.local/share/containers/storage"))
                 (passwd:uid account) (passwd:gid account))))
       users))

(define (same-directory? source target)
  (let ((source (stat source)) (target (stat target)))
    (and (= (stat:dev source) (stat:dev target))
         (= (stat:ino source) (stat:ino target)))))

(define (directory! path uid gid mode)
  ;; Change ownership only when creating a directory, never in recovered data.
  (if (file-exists? path)
      (unless (eq? 'directory (stat:type (lstat path)))
        (error "Podman storage path must be a real directory" path))
      (begin
        (directory! (dirname path) uid gid mode)
        (mkdir path mode)
        (chown path uid gid))))

(define (empty-or-absent? path)
  (or (not (file-exists? path))
      (and (eq? 'directory (stat:type (lstat path)))
           (every (lambda (name) (member name '("." "..")))
                  (scandir path)))))

(define (unmount-binding source target)
  (when (member target (mount-points))
    (unless (same-directory? source target)
      (error "Refusing to unmount unrelated Podman storage" target))
    (umount target)))

(define (mount-podman-storage directory users)
  "Bind USERS' existing graph-root paths to DIRECTORY/podman/USER/storage.
Require DIRECTORY to be mounted, refuse to hide existing data, and undo only
mounts created by this invocation if a later mount fails."
  (let ((created '()))
    (catch #t
      (lambda ()
        (unless (member directory (mount-points))
          (error "OCI storage filesystem is not mounted" directory))
        (let ((stores (bindings directory users)))
          (for-each
           (match-lambda
             ((source target uid gid)
              (when (file-exists? source)
                (unless (and (eq? 'directory (stat:type (lstat source)))
                             (= uid (stat:uid (stat source))))
                  (error "Podman backing directory has the wrong owner or type" source)))
              (if (member target (mount-points))
                  (unless (and (file-exists? source) (same-directory? source target))
                    (error "Podman graph root already has another mount" target))
                  (unless (empty-or-absent? target)
                    (error "Migrate existing Podman graph root before binding" target)))))
           stores)
          (directory! (string-append directory "/podman") 0 0 #o755)
          (for-each
           (match-lambda
             ((source target uid gid)
              (unless (member target (mount-points))
                (directory! source uid gid #o700)
                (directory! target uid gid #o700)
                (mount source target #f MS_BIND)
                (set! created (cons (list source target) created)))))
           stores))
        #t)
      (lambda (key . arguments)
        (for-each (lambda (binding) (apply unmount-binding binding)) created)
        (format (current-error-port) "Podman storage start failed: ~s ~s~%" key arguments)
        #f))))

(define (unmount-podman-storage directory users)
  "Unmount only our bindings; already absent mounts are harmless."
  (for-each
   (match-lambda
     ((source target uid gid) (unmount-binding source target)))
   (reverse (bindings directory users)))
  #f)
