(import scheme
        chicken.base
        chicken.condition)
(import srfi-18)

(define log-port (make-parameter (current-error-port)))

(define (logger context . args)
  (display (string-append "[" context "] ") (log-port))
  (let loop ((rest args))
    (if (null? rest)
        (newline (log-port))
        (begin
          (display (car rest) (log-port))
          (display " " (log-port))
          (loop (cdr rest))))))

(define get-exn-msg (condition-property-accessor 'exn 'message))

(define (time-stamp)
  (time->seconds (current-time)))
