(define-class <message-format> () ())

(define-generic (rpc-message-format? obj)
  #f)
(define-method (rpc-message-format? (obj <message-format>)) #t)

; format the given message and return a blob
(define-method (rpc-format-request msg) '())
(define-method (rpc-format-notification msg) '())
(define-method (rpc-format-response msg) '())

; write the given message to the port
(define-method (rpc-write-request port msg) '())
(define-method (rpc-write-notification port msg) '())
(define-method (rpc-write-response port msg) '())

; parse the blob as a rpc message and return a list
(define-method (rpc-parse blob) '())

; read from port a rpc message and return a list
(define-method (rpc-read port) '())
