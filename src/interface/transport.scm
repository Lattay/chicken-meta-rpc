; server side
(define-class <transport-server> () ())

(define-generic (transport-server? obj) #f)
(define-method (transport-server? (obj <transport-server>)) #t)


(define-method (one-shot? (obj <transport-server>) #t))
(define-method (ready? (obj <transport-server>) #f))
(define-method (accept (obj <transport-server>) (values #f #f)))
(define-method (multiple-client? (obj <transport-server>) #t))

; client side
(define-class <transport-client> () ())

(define-generic (transport-client? obj) #f)
(define-method (transport-client? (obj <transport-client>)) #t)

(define-method (one-shot? (obj <transport-client>) #t))
(define-method (connect (obj <transport-client>) (values #f #f)))
