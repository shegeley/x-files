(define-module (x-files utils alist)
  #:use-module (guix build utils)

  #:use-module (gnu services configuration)

  #:use-module (x-files utils base)

  #:export-syntax (match-alist))

(define-syntax match-alist-inner
  (lambda (s)
    (syntax-case s ()
      ((_ alist ((reference variable) rest ...)
          body ...)
       #'(let* ((variable (ref-in alist reference)))
           (match-alist-inner alist (rest ...) body ...)))
      ((_ alist (reference rest ...)
          body ...)
       #'(match-alist-inner alist
          (('(reference) reference) rest ...)
          body ...))
      ((_ alist () body ...)
       #'(begin body ...)))))

(define-syntax match-alist
  (syntax-rules ()
    ((_ alist (keys ...) body ...)
     (if (alist? alist)
         (match-alist-inner alist (keys ...) body ...)
         (throw 'wrong-type-arg alist)))))
