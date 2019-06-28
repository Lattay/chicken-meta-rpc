(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18)

(load "test/test-pseudo-transport.scm")
(load "test/test-pseudo-format.scm")
(load "meta-rpc.so")
(import meta-rpc)


(define transport (make-pseudo-transport #f))
(define msg-format (make-pseudo-format))
(define client (make-client transport msg-format))
(define-values (server events) (make-server transport msg-format))

(define foo-a (make-parameter #f))
(define foo-b (make-parameter #f))
(define bar (make-parameter 0))
(define done (make-parameter #f))



(hash-table-set! *rpc-methods* "foo" (lambda (a b)
                                       (foo-a a)
                                       (foo-b b)
                                       '(b a)))

(hash-table-set! *rpc-methods* "bar" (lambda ()
                                       (bar (add1 (bar)))
                                       (bar)))

(client-wait-timeout 5)

; start the server
(thread-start! server)

(test-group "pseudo-format pseudo-transport"
    (define-syntax test-sync-call-foo
      (syntax-rules ()
        ((_ client da db a b)
         (test-group (format "sync-call foo ~A ~A" a b)
                     (test "result" `(,b ,a) (client 'sync-call "foo" `(,a ,b)))
                     (test "a" a (da))
                     (test "b" a (db))))))

    (test-sync-call-foo client foo-a foo-b 4 5)
    (test-sync-call-foo client foo-a foo-b '() '())
    (test-sync-call-foo client foo-a foo-b "ok" "ko")
    (test-sync-call-foo client foo-a foo-b '(1 2 3) '(4 5 6))
    (test-sync-call-foo client foo-a foo-b 'patate 'poulet))

