(import scheme chicken.base)
(import coops)
(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(define-class <pseudo-format> (<message-format>))
(define (make-pseudo-format)
  (make <pseudo-format>))

; write the given message to the port
(define-method (rpc-write-request (fmt <pseudo-format>) port msg)
  (with-current-output-port port
                            (lambda ()
                              (print (cons 'request msg)))))
(define-method (rpc-write-notification (fmt <pseudo-format>) port msg)
  (with-current-output-port port 
                            (lambda ()
                              (print (cons 'notification msg)))))

(define-method (rpc-write-response (fmt <pseudo-format>) port msg)
  (with-current-output-port port
                            (lambda ()
                              (print (cons 'response msg)))))

; read from port a rpc message and return a list
(define-method (rpc-read (fmt <pseudo-format>) port)
  (with-current-input-port port read))
