(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18 mailbox)

(load "test/tunnel.scm")
(load "test/pseudo-transport.scm")
(load "test/pseudo-format.scm")

(include "src/main/common.scm")
(include "src/main/client.scm")

(define msg-format (make-pseudo-format))

(define responses (make-hash-table))
(define mb (make-mailbox))
(define-values (in out) (make-tunnel-port))

(define wait (client-waiter transport msg-format one-co connection.s
                                requests responses events))

(test-group "test client"
;  (test-group "client-wait"
;    (test "instant" '(result . that-s-it)
;      (begin
;        (set-response! responses 2 result: 'that-s-it)
;        (client-wait responses 2)))
;
;    (test "a bit" '(result . yes-papa)
;      (begin
;        (thread-start! (lambda ()
;                         (thread-sleep! 1)
;                         (set-response! responses 2 result: 'yes-papa)))
;        (client-wait responses 2)))
;    (client-wait-timeout 1)
;    (test-error "too much"
;                (begin
;                  (thread-start! (lambda ()
;                                   (thread-sleep! 2)
;                                   (set-response! responses 5 result: 'no-way)))
;                  (client-wait responses 2)))
;    (test "all responses treated" 0 (hash-table-size responses)))
;  (test-group "client-request"
;    (test "request" '(request 3 "foo" (a b c))
;      (begin
;        (client-request mb 'request 3 msg-format "foo" '(a b c))
;        (let ((res (mailbox-receive! mb)))
;          ((cdr res) out)
;          (rpc-read msg-format in))))
;    (test "notification" '(notification "foo" (a b c))
;      (begin
;        (client-request mb 'notify 3 msg-format "foo" '(a b c))
;        (let ((res (mailbox-receive! mb)))
;          ((cdr res) out)
;          (rpc-read msg-format in)))))
;  (test-group "send-requests"
;    (define transport (make-pseudo-transport #f))
;    (test "one-co: connected done no-error" '(#t #t #f)
;      (begin
;        (define done (make-parameter #f))
;        (define err (make-parameter #f))
;        (send-request-one-co transport
;                             (lambda (port) (done #t))
;                             (lambda (e) (err e))
;                             (make-parameter #f))
;        (list (ready? transport) (done) (err))))
;    (define transport (make-pseudo-transport #f))
;    (test "multi-co: connected done no-error" '(#t #t #f)
;      (begin
;        (define done (make-parameter #f))
;        (define err (make-parameter #f))
;        (mailbox-send! mb (cons 1 (lambda (port) (done #t))))
;        (send-requests-multi-co transport
;                                (lambda (port) (done #t))
;                                (lambda (e) (err e))
;                                (make-hash-table))
;        (list (ready? transport) (done) (err)))))
;  (test-group "receive-responses"
;    (define transport (make-pseudo-transport #f))
;    (define resp (make-hash-table))
;    (define-values (in out) (make-tunnel-port))
;    (rpc-write-response msg-format out (list 5 '() '(1 2 3)))
;    (rpc-write-response msg-format out (list 6 '() '(2 2 3)))
;    (assert (char-ready? in))
;    (test "one-co response: " '(() . (1 2 3))
;      (begin
;        (receive-responses-one-co resp mb (make-parameter (cons in out)) msg-format)
;        (get-response resp 5)))
;    (test "multi-co response: " '(() . (2 2 3))
;      (begin
;        (receive-responses-multi-co resp mb (make-hash-table) msg-format)
;        (get-response resp 6)))
;    )
  )
