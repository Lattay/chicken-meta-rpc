(import scheme
        chicken.base
        chicken.format
        chicken.io)

(import test srfi-69 srfi-18 mailbox)

(include "test/tunnel.scm")
(include "test/pseudo-transport.scm")
(include "test/pseudo-format.scm")

(define (flush-input port)
  (when (char-ready? port)
    (read-char port)
    (flush-input port)))

(test-group "test-test"
  (test-group "tunnel"
    (define-values (in out) (make-tunnel-port))
    (define-syntax test-write-read
      (syntax-rules ()
        ((_ name msg)
         (test (format "write-read ~A" name) msg
           (begin
             (write-string msg #f out)
             (list->string
               (reverse
                 (let loop ((acc '()))
                   (let ((c (read-char in)))
                     (if (eq? #!eof c)
                         acc
                         (loop (cons c acc))))))))))))
    (test-write-read "1" "hello")
    (test "not char-ready" #f (char-ready? in))
    (test "char-ready" #t (begin (write-char #\o out) (char-ready? in)))
    (flush-input in)
    (test-write-read "2" "such test, much code")
    (test-write-read "unicode" "Hébété, nous ne pûmes réagirent.")
    )

  (test-group "format"
    (define fmt (make-pseudo-format))
    (define-values (in out) (make-tunnel-port))
    (test "write-read request" (list 'request 2 "foo" '(5))
      (begin
        (rpc-write-request fmt out (list 2 "foo" '(5)))
        (rpc-read fmt in)))
    (flush-input in)
    (test-error "read error" (rpc-read fmt in))
    (test-error "read invalid"
                (begin
                  (rpc-write-request fmt out (list "foo"))
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
