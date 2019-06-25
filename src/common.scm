(import scheme
        chicken.base
        chicken.io
        chicken.condition)
(import srfi-18
        srfi-69)

(define-syntax decr!
  (syntax-rules ()
    ((_ v)
     (set! v (sub1 v)))
    ((_ v amount)
     (set! v (- v amount)))))

(define-syntax incr!
  (syntax-rules ()
    ((_ v)
     (set! v (add1 v)))
    ((_ v amount)
     (set! v (+ v amount)))))

(define (logger context . args)
  (display (string-append "[" context "] ") (current-error-port))
  (let loop ((rest args))
    (if (null? rest)
        (newline (current-error-port))
        (begin
          (display (car rest) (current-error-port))
          (display " ")
          (loop (cdr rest))))))

(define get-exn-msg (condition-property-accessor 'exn 'message))

(define-syntax log-errors
  (syntax-rules ()
    ((_ ctx first . body)
     (let ((res #f))
       (condition-case (set! res (begin first . body))
                       (e (exn i/o net timeout)
                          (logger ctx "Timeout reached:" (get-exn-msg e)))
                       (e (exn)
                          (logger ctx "Error encountered:" (get-exn-msg e))))
       res))))

(define-syntax error-as-msg
  (syntax-rules ()
    ((_ ((err res) expr) body ...)
     (let ((res #f) (err #f))
       (condition-case (set! res expr)
                       (e (exn arity) (set! err 
                                        (string-append 
                                          "Wrong number of argument: "
                                          (get-exn-msg e))))
                       (e (exn type) (set! err
                                       (string-append
                                         "Type error: "
                                         (get-exn-msg e))))
                       (e (exn) (set! err
                                  (string-append
                                    "Error: "
                                    (get-exn-msg e)))))
       body ...))))

(define (time)
  (time->seconds (current-time)))
