(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18 mailbox)

(load "test/tunnel.scm")
(load "test/test-pseudo-transport.scm")
(load "test/test-pseudo-format.scm")

(include "src/main/common.scm")
(include "src/main/server.scm")
(include "src/main/client.scm")

(define transport (make-pseudo-transport #f))
(define msg-format (make-pseudo-format))
(define client (make-client transport msg-format))

(define foo-a (make-parameter #f))
(define foo-b (make-parameter #f))
(define bar (make-parameter 0))
(define done (make-parameter #f))

(test-group "full-client"
  )

(test-group "no-client"
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

  (test-group "server internal"
    (define-values (in1 out1) (make-tunnel-port))
    (define-values (in2 out2) (make-tunnel-port))
    (define-values (in3 out3) (make-tunnel-port))

    (test "first-ready" (,in2 . ,out2)
      (let ((connections `((,in1 . ,out1) (,in2 . ,out2) (,in3 . ,out3))))
        (rpc-write-notification msg-format out2 (list 5 "foo" '(5 4 6)))
        (let-values (((co rest) (first-ready connections)))
                    co)))
      ))
