(import scheme
        chicken.base
        chicken.condition)
(import srfi-18)

(define log-port (make-parameter (current-error-port)))

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
  (display (string-append "[" context "] ") (log-port))
  (let loop ((rest args))
    (if (null? rest)
        (newline (log-port))
        (begin
          (display (car rest) (log-port))
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

(define (triplet a b c)
  `(,a ,b . ,c))

(define (time-stamp)
  (time->seconds (current-time)))
