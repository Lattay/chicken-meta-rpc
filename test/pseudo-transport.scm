(import scheme chicken.base)
(import srfi-69)
(import coops)
(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(load "test/tunnel.scm")

(define-class <pseudo-transport> (<transport-client> <transport-server>)
  (in-serv
   out-serv
   in-cli
   out-cli
   (ready #f)))

(define (make-pseudo-transport)
  (let-values (((in-serv out-cli) (make-tunnel-port))
               ((in-cli out-serv) (make-tunnel-port)))
    (make <pseudo-transport> 
          'in-serv in-serv
          'out-serv out-serv
          'in-cli in-cli
          'out-cli out-cli)))

(define-method (one-shot? (o <pseudo-transport>))
  #f)

(define-method (ready? (o <pseudo-transport>))
  (slot-value o 'ready))

(define-method (accept (o <pseudo-transport>))
  (set! (slot-value o 'ready) #f)
  (values
    (slot-value o 'in-serv)
    (slot-value o 'out-serv)))
    
(define-method (connect (o <pseudo-transport>))
  (set! (slot-value o 'ready) #t)
  (values
    (slot-value o 'in-cli)
    (slot-value o 'out-cli)))

;;;;;;;;;;;;;;;;;;;;;;;
(define-class <pseudo-transport-multi-co> (<transport-client> <transport-server>)
  ((ready #f)
   (connections '())))

(define (make-pseudo-transport-multi-co)
    (make <pseudo-transport-multi-co>))

(define-method (one-shot? (o <pseudo-transport-multi-co>))
  #t)

(define-method (ready? (o <pseudo-transport-multi-co>))
  (slot-value o 'ready))

(define-method (accept (o <pseudo-transport-multi-co>))
  (if (ready? o)
      (let ((co (car (slot-value o 'connections))))
        (set! (slot-value o 'ready) #f)
        (values (car co) (cdr co)))
      (begin
        (error "Transport not ready.")
        (values #f #f))))
    
(define-method (connect (o <pseudo-transport-multi-co>))
  (let-values (((in-serv out-cli) (make-tunnel-port))
               ((in-cli out-serv) (make-tunnel-port)))
    (set! (slot-value o 'ready) #t)
    (set! (slot-value o 'connections) (cons (cons in-serv out-serv) (slot-value o 'connections)))
    (values in-cli out-cli)))

