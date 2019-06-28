(import scheme chicken.base chicken.port)

(define (make-tunnel-port)
  (let ((buffer '())
        (rev-buff '())
        (closed #f))
    (let ((this-write (lambda (char)
                        (set! buffer (cons char buffer))))
          (this-read (lambda ()
                       (if closed
                           #!eof
                           (if (null? rev-buff)
                               (if (null? buffer)
                                   #!eof
                                   (let ((rev-tmp (reverse buffer)))
                                     (set! rev-buff (cdr rev-tmp))
                                     (set! buffer '())
                                     (car rev-tmp)))
                               (let ((tmp (car rev-buff)))
                                 (set! rev-buff (cdr rev-buff))
                                 tmp)))))
          (this-ready? (lambda ()
                         (not (and closed (null? rev-buff) (null? buffer)))))
          (this-close (lambda ()
                        (set! closed #f))))
      (values
        (make-input-port this-read this-ready? this-close)
        (make-output-port this-write this-close)))))

