(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18)

(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(load "test/pseudo-transport.scm")
(load "test/pseudo-format.scm")
(load "meta-rpc.so")
(import meta-rpc)

(load "test/tunnel.scm")


(define transport (make-pseudo-transport #f))
(define msg-format (make-pseudo-format))

(define (make-multi-thread-parameter value)
  (let ((val value))
        (lambda arg
          (when (not (null? arg))
              (set! val (car arg)))
          val)))

(define foo-a (make-multi-thread-parameter #f))
(define foo-b (make-multi-thread-parameter #f))
(define bar (make-multi-thread-parameter 0))

(hash-table-set! *rpc-methods* "foo" (lambda (a b)
                                       (foo-a a)
                                       (foo-b b)
                                       (list b a)))

(hash-table-set! *rpc-methods* "bar" (lambda ()
                                       (bar (add1 (bar)))))

(client-wait-timeout 2)

(define (wait-ready transport n)
  (if (zero? n)
      (error "Timeout waiting for client to connect")
      (begin
        (thread-sleep! 1)
        (if (ready? transport)
            '()
            (wait-ready transport (sub1 n))))))

; start the server

(test-group "tests"
  (test-group "client"
    (define client (make-client transport msg-format))
    (test "send request" '(request 1 "foo" (1 2))
      (begin
        (client 'call "foo" '(1 2))
        (wait-ready transport 5)
        (let-values (((in out) (accept transport)))
          (let loop ((retry 5))
            (if (zero? retry)
                'timeout
                (if (char-ready? in)
                    (rpc-read msg-format in)
                    (begin
                      (thread-sleep! 1)
                      (loop (sub1 retry)))))))))
    (test "wait response" '(() (2 1))
      (begin
        (let-values (((in out) (accept transport)))
          (rpc-write-response msg-format out '(1 "foo" () (2 1)))
          (client 'wait 1))))
    (define client (make-client transport msg-format))
    (test "send notification" '(notification "foo" (1 2))
      (begin
        (client 'notify "foo" '(1 2))
        (thread-sleep! 1)
        (wait-ready transport 5)
        (let-values (((in out) (accept transport)))
          (let loop ((retry 5))
            (if (zero? retry)
                'timeout
                (if (char-ready? in)
                    (rpc-read msg-format in)
                    (begin
                      (thread-sleep! 1)
                      (loop (sub1 retry)))))))))
    )
  (test-group "server")
  (test-group "integrated"
    (define client (make-client transport msg-format))
    (define-values (server events) (make-server transport msg-format))
    (thread-start! server)
    (define-syntax test-sync-call-foo
      (syntax-rules ()
        ((_ client da db a b)
         (test-group (format "sync-call foo ~A ~A" a b)
           (test "result" `(() (,b ,a)) (client 'sync-call "foo" (list a b)))
           (test "a" a (da))
           (test "b" b (db))))))

    (test-sync-call-foo client foo-a foo-b 4 5)
    (test-sync-call-foo client foo-a foo-b '() '())
    (test-sync-call-foo client foo-a foo-b "ok" "ko")
    (test-sync-call-foo client foo-a foo-b '(1 2 3) '(4 5 6))
    (test-sync-call-foo client foo-a foo-b 'patate 'poulet)

    (test-group "sync-call bar"
      (define-syntax test-sync-call-bar
        (syntax-rules ()
          ((_ client count)
           (test (format "#~A" count) (list '() count) (client 'sync-call "bar" '())))))
      (test-sync-call-bar client 1)
      (test-sync-call-bar client 2)
      (test-sync-call-bar client 3)
      (test-sync-call-bar client 4))
    )
  )
