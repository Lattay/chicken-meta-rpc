(define-class <message-format>)

(define-method (message-format? obj) #f)
(define-method (message-format? (obj <message-format>)) #t)

; format the given message and return a blob
; (define-method (rpc-format-request (fmt <message-format>) msg) '())
; (define-method (rpc-format-notification (fmt <message-format>) msg) '())
; (define-method (rpc-format-response (fmt <message-format>) msg) '())

; write the given message to the port
(define-method (rpc-write-request (fmt <message-format>) port msg) '())
(define-method (rpc-write-notification (fmt <message-format>) port msg) '())
(define-method (rpc-write-response (fmt <message-format>) port msg) '())

; parse the blob as a rpc message and return a list
; (define-method (rpc-parse (fmt <message-format>) blob) '())

; read from port a rpc message and return a list
(define-method (rpc-read (fmt <message-format>) port) '())
