(import scheme
        chicken.base
        chicken.format
        chicken.io)

(import test srfi-69 srfi-18 mailbox)

(include "test/tunnel.scm")
(include "test/pseudo-transport.scm")
(include "test/pseudo-format.scm")

(test-group "test-test"
  (test-group "tunnel"
    (define-values (in out) (make-tunnel-port))
    (test "write-read" '(#\o #\l #\l #\e #\h)
      (begin
        (write-char #\h out)
        (write-char #\e out)
        (write-char #\l out)
        (write-char #\l out)
        (write-char #\o out)
        (let loop ((acc '()))
          (let ((c (read-char in)))
            (if (eq? #!eof c)
              acc
              (loop (cons c acc)))))))
    (test "not char-ready" #f (char-ready? in))
    (test "char-ready" #t (begin (write-char #\o out) (char-ready? in)))
    )

  (test-group "format"
    (define fmt (make-pseudo-format))
    (define-values (in out) (make-tunnel-port))
    (test "write-read request" (list 'request 2 "foo" '() '(5))
      (begin
        (rpc-write-request fmt out (list 2 "foo" '() '(5)))
        (rpc-read fmt in))))

  (test-group "transport"
    (define tr (make-pseudo-transport #f))
    (test "not ready" #f (ready? tr))
    (define-values (in-cli out-cli) (connect tr))
    (test "ready" #t (ready? tr))
    (define-values (in-serv out-serv) (accept tr))
    (test "server-to-client-write" '(truc "foo" 56)
      (begin
        (write '(truc "foo" 56) out-serv)
        (read in-cli)))
    (test "server-to-client-string" "hello"
      (begin
        (write-string "hello" #f out-serv)
        (read-string #f in-cli)))

    (test "client-to-server-write" '(truc "foo" 56)
      (begin
        (write '(truc "foo" 56) out-cli)
        (read in-serv)))
    (test "client-to-server-string" "hello"
      (begin
        (write-string "hello" #f out-cli)
        (read-string #f in-serv)))))
