(import chicken.tcp)

(define +max-co-default+ 5)
(define +one-shot-default+ 5)

; server side
(define-class <transport-server-tcp> (<transport-server>)
  (host port listener (max-co +max-co-default+) (one-shot +one-shot-default+)))

(define (make-transport-server-tcp host port #!key (max-co +max-co-default+) (one-shot +one-shot-default+))
  (let ((listener (tcp-listen port max-co host)))
    (make <transport-server-tcp> 'host host 'port port 'max-co max-co 'one-shot one-shot 'listener listener)))
 
(define-method (multiple-client? (serv <transport-server-tcp>)) #t)

(define-method (one-shot? (serv <transport-server-tcp>))
  (slot-value serv 'one-shot))

(define-method (ready? (serv <transport-server-tcp>))
  (tcp-accept-ready? (slot-value serv 'listener)))

(define-method (accept (serv <transport-server-tcp>))
  (tcp-accept (slot-value serv 'listener)))

(define-method (shutdown (serv <transport-server-tcp>))
  (tcp-close (slot-value serv 'listener)))

; client side
(define-class <transport-client-tcp> (<transport-client>)
  (host port (one-shot #f)))

(define (make-transport-client-tcp host port #!key (one-shot +one-shot-default+))
  (make <transport-client-tcp> 'host host 'port port 'one-shot one-shot))
  
(define-method (one-shot? (cli <transport-client-tcp>))
  (slot-value cli 'one-shot))

(define-method (connect (cli <transport-client-tcp>))
  (tcp-connect (slot-value cli 'host) (slot-value cli 'port)))
