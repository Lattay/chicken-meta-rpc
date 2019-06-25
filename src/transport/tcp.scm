(import scheme
        chicken.tcp)

(define (make-tcp-server host port max-co . stable)
  (let ((listener (tcp-listen port max-co host)))
    (lambda (method)
      (case method
        (('multiple-client?)
         #t)
        (('one-shot?)
         (or (null? stable) (not (car stable))))
        (('ready?)
         (tcp-accept-ready? listener))
        (('accept)
         (tcp-accept listenener))))))

(define (make-tcp-client host port . stable)
  (lambda (method)
    (case method
      (('one-shot?)
       (or (null? stable) (not (car stable))))
      (('connect)
       (tcp-connect host port)))))
