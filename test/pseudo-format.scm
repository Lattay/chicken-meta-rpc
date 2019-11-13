(import scheme chicken.base chicken.format chicken.condition)
(import coops)
(load "meta-rpc.interface.so")
(import meta-rpc.interface)
(import matchable)

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

(define (validate msg)
  (match msg
    (('request id method-name params) msg)
    (('notification method-name params) msg)
    (('response id error result) msg)
    (any (signal (condition `(exn location rpc-read-validation
                                  message ,(format "Invalid message ~A" msg))
                            '(rpc)
                            '(invalid))))))

; read from port a rpc message and return a list
(define-method (rpc-read (fmt <pseudo-format>) port)
  (validate (read port)))
