(import scheme)
(import matchable)

(define (make-fifo-transport infile outfile #!optional one-shot multi-client)
  (lambda (mode)
    (case mode
      (('multiple-client?)
       #f)
      (('one-shot?)
       one-shot)
      (('ready?)
        (if ready
            (begin
              (set! ready #f)
              #t)
            #f))
      (('connect)
       (let ((in (open infile)) (out (open outfile)))
         (values in out)))
      (('accept)
       (let ((in (open infile)) (out (open outfile)))
         (values in out))))))

(define (make-stdio-transport infile outfile)
  ; provide both server and client interface
  (let ((ready #t))
    (lambda (mode)
      (case mode
        (('multiple-client?)
         #f)
        (('one-shot?)
         #f)
        (('ready?)
         ready)
        (('connect)
         (values (current-input-port) (current-output-port)))
        (('accept)
         (if ready
             (begin
               (set! ready #f)
               (values (current-input-port) (current-output-port))))
             (values #f #f))))))
