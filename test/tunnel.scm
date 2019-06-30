(import scheme chicken.base chicken.port)

(define (make-tunnel-port)
  (let ((buffer '())
        (rev-buff '())
        (closed #f))
    (let ((this-write (lambda (str)
                        (unless closed
                          (assert (and 'write-char (string? str)))
                          (let loop ((rest (string->list str)))
                            (if (null? rest)
                              '()
                              (begin 
                                (set! buffer (cons (car rest) buffer))
                                (loop (cdr rest))))))))
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
                         (and (not closed) (not (and (null? rev-buff) (null? buffer))))))
          (this-close (lambda ()
                        (set! closed #t))))
      (values
        (make-input-port this-read this-ready? this-close)
        (make-output-port this-write this-close)))))

