(import scheme chicken.base chicken.format chicken.io)
(import test srfi-69 srfi-18 mailbox)

(load "test/tunnel.scm")
(load "test/pseudo-transport.scm")
(load "test/pseudo-format.scm")

(include "src/main/common.scm")
(include "src/main/server.scm")


; debug actor
(define-class <debug> (<actor>)
  ((logs (make-mailbox))))

(define-method (work (self <debug>) timeout)
  (let ((start (time-stamp)))
    (let loop ()
      (let ((m (mailbox-receive! (slot-value self 'private-mailbox) 1 #f)))
        (if m
            (handle self (car m) (cdr m))))
      (when (> timeout (- (time-stamp) start))
        (thread-sleep! 0.05)
        (loop)))))

(define-method (handle (self <debug>) msg data)
  ; (display (format "\nReceived message ~A with data ~A\n" msg data))
  (mailbox-send! (slot-value self 'logs) (cons msg data)))

(define-method (flush-log (self <debug>))
  (unless (mailbox-empty? (slot-value self 'logs))
    (mailbox-receive! (slot-value self 'logs))
    (flush-log self)))

(define-method (next-log (self <debug>))
  (if (mailbox-empty? (slot-value self 'logs))
      #f
      (mailbox-receive! (slot-value self 'logs))))

;
(define (make-multi-thread-parameter value)
  (let ((val value))
        (lambda arg
          (when (not (null? arg))
              (set! val (car arg)))
          val)))

(define (flush-input port)
  (when (char-ready? port)
    (read-char port)
    (flush-input port)))

(define msg-format (make-pseudo-format))
(define debug (make <debug>))
(define-values (in out) (make-tunnel-port))

(define foo-a (make-multi-thread-parameter #f))
(define foo-b (make-multi-thread-parameter #f))
(hash-table-set! *rpc-methods* "foo" (lambda (a b)
                                       (foo-a a)
                                       (foo-b b)
                                       (list b a)))
(hash-table-set! *rpc-methods* "jerk"
                 (lambda ()
                   (signal (condition '(exn location jerk)
                                      '(rpc)
                                      '(app code 42 data (pouet) message "damned")))))

(test-group "test-server"
  (test-group "internals"
    (test "call-method" (list '() '(4 6))
      (call-method "foo" '(6 4)))

    (test "call-method error" -32601
      (let ((res (call-method "bar" '(7 8))))
        (assert (hash-table? (car res)))
        (hash-table-ref/default (car res) "code" #f)))

    (test "call-method app error" '(42 (pouet) "damned")
      (let ((res (call-method "jerk" '())))
        (assert (hash-table? (car res)))
        (list
          (hash-table-ref/default (car res) "code" #f)
          (hash-table-ref/default (car res) "data" #f)
          (hash-table-ref/default (car res) "message" #f))))

    (test "handle-request" (list 5 "foo" '() '(3 2))
      (handle-request (list 'request 5 "foo" '(2 3))))

    (test "handle-request" '()
      (handle-request (list 'notification "foo" '(2 3))))

    (test "read-message" '(#f (request 1 "foo" (1 2)))
      (begin
        (rpc-write-request msg-format out '(1 "foo" (1 2)))
        (let-values (((err res) (read-message msg-format in)))
          (list err res))))

    (test "read-message err" '(-32000 #f)
      (begin
        (write-string "(request \"foo\"" #f out)
        (let-values (((err res) (read-message msg-format in)))
          (assert (hash-table? err))
          (list 
            (hash-table-ref/default err "code" #f)
            res))))

    (flush-input in)
    (test "read-message invalid message err" '(-32700 #f)
      (begin
        (write '(request "foo" (1 2)) out)
        (let-values (((err res) (read-message msg-format in)))
          (assert (hash-table? err))
          (list 
            (hash-table-ref/default err "code" #f)
            res))))
    )

  (test-group "worker"
    (define wk (make-worker debug msg-format))
    (thread-start! (lambda () (work wk)))
    (test "message" '(1 2 (response 1 "foo" () (2 1)))
      (let ()
        (flush-input in)
        (rpc-write-request msg-format out '(1 "foo" (1 2)))
        (send wk 'new-message `(0 . ,(make-conn in out)))
        (work debug 2)
        (list (foo-a) (foo-b) (rpc-read msg-format in))))
    (flush-log debug)
    (test "unparsable message" '(task-error -32000)
      (let ()
        (flush-input in)
        (write-string "(\"a\" \"b\" \"c\"" #f out)
        (send wk 'new-message `(0 . ,(make-conn in out)))
        (work debug 2)
        (let ((logged (next-log debug)))
          (list
            (car logged)
            (hash-table-ref/default (cdddr logged) "code" #f)))))
    (flush-log debug)
    (test "invalid message" '(task-error -32700)
      (let ()
        (flush-input in)
        (write '(request "foo") out)
        (send wk 'new-message `(0 . ,(make-conn in out)))
        (work debug 2)
        (let ((logged (next-log debug)))
          (list
            (car logged)
            (hash-table-ref/default (cdddr logged) "code" #f)))))
    (send wk 'stop '()))
  )
