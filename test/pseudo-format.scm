(import scheme chicken.base)
(import coops)
(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(define-class <pseudo-format> (<message-format>))
(define (make-pseudo-format)
  (make <pseudo-format>))

; write the given message to the port
(define-method (rpc-write-request (fmt <pseudo-format>) port msg)
  (write (cons 'request msg) port))

(define-method (rpc-write-notification (fmt <pseudo-format>) port msg)
  (write (cons 'notification msg) port))

(define-method (rpc-write-response (fmt <pseudo-format>) port msg)
  (write (cons 'response msg) port))

; read from port a rpc message and return a list
(define-method (rpc-read (fmt <pseudo-format>) port)
  (read port))
