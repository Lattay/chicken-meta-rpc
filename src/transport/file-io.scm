; server side
(define-class <transport-server-file> (<transport-server>)
  ((ready #f)))

(define-method (multiple-client? (serv <transport-server-tcp>)) #f)

(define-method (one-shot? (serv <transport-server-file>))
  #f)

(define-method (ready? (serv <transport-server-file>))
  (slot-value serv 'ready))

; fifo specific
(define-class <transport-server-fifo> (<transport-server-file>)
  (infile outfile))

(define (make-transport-server-fifo infile outfile)
  (make <transport-server-fifo> 'infile infile 'outfile 'outfile))

(define-method (accept (serv <transport-server-fifo>))
  (set! (slot-value serv 'ready) #f)
  (values
    (open-input-file (slot-value serv 'infile))
    (open-output-file (slot-value serv 'outfile))))

; stdio specific
(define-class <transport-server-stdio> (<transport-server-file>))

(define (make-transport-server-stdio)
  (make <transport-server-fifo>))

(define-method (accept (serv <transport-server-stdio>))
  (set! (slot-value serv 'ready) #f)
  (values
    (current-input-port)
    (current-output-port)))

; client side
(define-class <transport-client-file> (<transport-client>))

(define-method (one-shot? (cli <transport-client-file>)) #f)

(define-class <transport-client-fifo> (<transport-client-file>)
  (infile outfile))

(define (make-transport-client-fifo infile outfile)
  (make <transport-client-fifo> 'infile infile 'outfile 'outfile))

(define-method (connect (cli <transport-client-stdio>))
  (values
    (open-input-file (slot-value cli 'infile))
    (open-output-file (slot-value cli 'outfile))))

; stdio specific
(define-class <transport-client-stdio> (<transport-client-file>))

(define (make-transport-client-stdio)
  (make <transport-client-stdio>))

(define-method (connect (cli <transport-client-stdio>))
  (values (current-input-port) (current-output-port)))
