(import scheme chicken.base chicken.format)
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
      (if (> timeout (- (time-stamp) start))
          (loop)))))

(define-method (handle (self <debug>) msg data)
  (let ((info (format "Received message ~A with data ~A\n" msg data)))
    (mailbox-send! (slot-value self 'logs) info)
    (display info)))

(define-method (flush-log (self <debug>))
  (let loop ()
    (unless (mailbox-empty? (slot-value self 'logs))
      (mailbox-receive! (slot-value self 'logs))
      (loop))))

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

(define msg-format (make-pseudo-format))
(define debug (make <debug>))
(define-values (in out) (make-tunnel-port))

(define foo-a (make-multi-thread-parameter #f))
(define foo-b (make-multi-thread-parameter #f))
(hash-table-set! *rpc-methods* "foo" (lambda (a b)
                                       (foo-a a)
                                       (foo-b b)
                                       (list b a)))

(test-group "test-server"
  (test-group "worker"
    (define wk (make-worker debug msg-format #f))
    (thread-start! (lambda () (work wk)))
    (test "message" '(response 1 "foo" () (2 1))
      (let ()
        (rpc-write-request msg-format out '(1 "foo" (1 2)))
        (send wk 'new-message (make-conn in out))
        (work debug 2)
        (rpc-read msg-format in)))
    (flush-log debug)
    (test "invalid message" "Received message task-error with data *"
      (let ()
        (rpc-write-request msg-format out '("a" "b" "c"))
        (send wk 'new-message (make-conn in out))
        (work debug 2)
        (next-log debug)))

    (send wk 'stop '())
    ))
