(import scheme chicken.base)
(import coops)
(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(load "test/tunnel.scm")

(define-class <pseudo-transport> (<transport-client> <transport-server>)
  ((one-shot #f)
   in-serv
   out-serv
   in-cli
   out-cli
   (first #f)))

(define (make-pseudo-transport one-shot)
  (let-values (((in-serv out-cli) (make-tunnel-port))
               ((in-cli out-serv) (make-tunnel-port)))
    (make <pseudo-transport> 
          'one-shot one-shot
          'in-serv in-serv
          'out-serv out-serv
          'in-cli in-cli
          'out-cli out-cli)))

(define-method (one-shot? (o <pseudo-transport>))
  (slot-value o 'one-shot))

(define-method (ready? (o <pseudo-transport>))
  (let ((tmp (slot-value o 'first)))
    (set! (slot-value o 'first) #f)
    tmp))

(define-method (accept (o <pseudo-transport>))
  (values
    (slot-value o 'in-serv)
    (slot-value o 'out-serv)))
    
(define-method (connect (o <pseudo-transport>))
  (values
    (slot-value o 'in-cli)
    (slot-value o 'out-cli)))

