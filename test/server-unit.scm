(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18 mailbox)

(load "test/tunnel.scm")
(load "test/pseudo-transport.scm")
(load "test/pseudo-format.scm")

(include "src/main/common.scm")
(include "src/main/server.scm")

(define msg-format (make-pseudo-format))

(test-group "test-server"
  (test-group "server sending"
    (define-values (in out) (make-tunnel-port))
    (define responses (make-mailbox))
    (define events (make-mailbox))

    (test "send-responses" (list 'response 5 "foo" '() '(5))
      (begin
        (mailbox-send! responses (cons out (list 5 "foo" '() '(5))))
        (send-responses responses #f msg-format)
        (let ((resp (rpc-read msg-format in)))
          resp)))

    (test "send-responses with errors" (list 'response 5 "bar" '(fatal) '())
      (begin
        (mailbox-send! responses (cons out (list 5 "bar" '(fatal) '())))
        (send-responses responses #f msg-format)
        (let ((resp (rpc-read msg-format in)))
          resp)))

    (test "send-events" (list 'notification "baz" '(4 5 6))
      (begin
        (mailbox-send! events (list "baz" 4 5 6))
        (send-events events (list (cons #f out)) msg-format)
        (rpc-read msg-format in))))

  (test-group "server internals"
    (define-values (in1 out1) (make-tunnel-port))
    (define-values (in2 out2) (make-tunnel-port))
    (define-values (in3 out3) (make-tunnel-port))
    (define connections (list
                          (cons in1 out1)
                          (cons in2 out2)
                          (cons in3 out3)))

    (test "first-ready" (cons in2 out2)
      (begin
        (rpc-write-notification msg-format out2 (list 5 "foo" '(5 4 6)))
        (rpc-write-notification msg-format out3 (list 5 "foo" '(5 4 6)))
        (let-values (((co rest) (first-ready connections)))
          co)))

    (test "sweep-connections" (cons (list (cons in2 out2)) 1)
      (begin
        (close-input-port in1)
        (close-input-port in3)
        (close-output-port out1)
        (close-output-port out3)
        (let-values (((kept n-kept) (sweep-connections connections)))
          (cons kept n-kept))))

    (test "call-method" (list '() '(5 4 6))
      (begin
        (hash-table-set! *rpc-methods* "foo" (lambda (a b c) (list c b a)))
        (call-method "foo" '(6 4 5))))

    (test "handle-request" (list 5 "foo" '() '(3 2 1))
      (handle-request (list 'request 5 "foo" '(1 2 3))))

    (test "handle-request notification" '()
      (handle-request (list 'notification "foo" '(1 2 3))))))
