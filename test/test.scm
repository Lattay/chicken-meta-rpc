(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18 coops)

(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(load "test/pseudo-transport.scm")
(load "test/pseudo-format.scm")

(load "meta-rpc.so")
(import meta-rpc)

(load "test/tunnel.scm")

(log-port (open-output-file "rpc-test.log"))

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

(client-wait-timeout 5)

(define (wait-ready transport n)
  (if (zero? n)
      (error "Timeout waiting for client to connect")
      (begin
        (thread-sleep! 1)
        (if (ready? transport)
            '()
            (wait-ready transport (sub1 n))))))

(test-group "all"
  (test-group "client"
    (test-group "transport multi-shot"
      (define transport (make-pseudo-transport))
      (define client (make-client transport msg-format))
      (test "send request" '(request 1 "foo" (1 2))
        (begin
          (client 'call "foo" 1 2)
          (let-values (((in out) (accept transport)))
            (rpc-read msg-format in))))
      (test "wait response" '(result . (2 1))
        (begin
          (let-values (((in out) (accept transport)))
            (rpc-write-response msg-format out '(1 () (2 1)))
            (client 'wait 1))))
      (test "send notification" '(notification "foo" (1 2))
        (begin
          (client 'notify "foo" 1 2)
          (let-values (((in out) (accept transport)))
            (if (char-ready? in)
                (rpc-read msg-format in)
                'no-response)))))

    ; (test-group "transport one-shot"
    ;   (define transport (make-pseudo-transport-multi-co))
    ;   (define client (make-client transport msg-format))
    ;   (test "not ready" #f (ready? transport))
    ;   (test "send request" '(request 1 "foo" (1 2))
    ;     (begin
    ;       (client 'call "foo" 1 2)
    ;       (let-values (((in out) (accept transport)))
    ;         (rpc-read msg-format in))))
    ;   (test "wait response" '(result . (8 4))
    ;     ; Il faut que je reconnecte le client et qu'il ai acces aux ports
    ;     (begin
    ;       (client 'call "foo" 2 3)
    ;       (let-values (((in out) (accept transport)))
    ;         (rpc-write-response msg-format out '(2 () (8 4)))
    ;         (client 'wait 2))))
    ;   (test "send notification" '(notification "foo" (1 2))
    ;     (begin
    ;       (client 'notify "foo" 1 2)
    ;       (let-values (((in out) (accept transport)))
    ;         (if (char-ready? in)
    ;             (rpc-read msg-format in)
    ;             'no-response)))))
    )

  (test-group "server"
    ; (log-msg (lambda (self type msg data)
    ;            (when (and (eq? type 'recv))

    ;              (display (list (class-of self) msg data))
    ;              (newline))))
    (define (debug co n-co n-th)
      (display (format "~A ~A\n" co n-th) (log-port)))
    (define transport (make-pseudo-transport))
    (define-values (server events stop-server) (make-server transport msg-format debug))
    (thread-start! (make-thread server 'server))
    (test "send-request" '(response 1 () (2 1))
      (let-values (((in out) (connect transport)))
        (rpc-write-request msg-format out '(1 "foo" (1 2)))
        (thread-sleep! 0.5)
        (if (char-ready? in)
            (rpc-read msg-format in)
            'no-response)))
    (stop-server))

  (test-group "integrated"
    (define (debug co n-co n-th)
      (display (format "~A ~A\n" co n-th) (log-port)))
    (define transport (make-pseudo-transport))
    (define client (make-client transport msg-format))
    (define-values (server events stop-server) (make-server transport msg-format debug))
    (thread-start! (make-thread server 'server))

    (test-group "sync-call foo"
      (define-syntax test-sync-call-foo
        (syntax-rules ()
          ((_ client da db a b)
           (test-group (format "sync-call foo ~A ~A" a b)
             (test "result" `(result . (,b ,a)) (client 'sync-call "foo" a b))
             (test "a" a (da))
             (test "b" b (db))))))
      (test-sync-call-foo client foo-a foo-b 4 5)
      (test-sync-call-foo client foo-a foo-b '() '())
      (test-sync-call-foo client foo-a foo-b "ok" "ko")
      (test-sync-call-foo client foo-a foo-b '(1 2 3) '(4 5 6))
      (test-sync-call-foo client foo-a foo-b 'patate 'poulet))

    (test-group "sync-call bar"
      (define-syntax test-sync-call-bar
        (syntax-rules ()
          ((_ client count)
           (test (format "#~A" count) `(result . ,count) (client 'sync-call "bar")))))
      (test-sync-call-bar client 1)
      (test-sync-call-bar client 2)
      (test-sync-call-bar client 3)
      (test-sync-call-bar client 4))

    (test-group "async-call foo" '()
      (test "two concurrent_call" '((result . (4 6)) (result . (5 1)))
            (let ((c1 (client 'call "foo" 6 4))
                  (c2 (client 'call "foo" 1 5)))
              (list
                (client 'wait c1)
                (client 'wait c2))))
      (test "six concurrent_call" '((result . (4 6))
                                    (result . (5 1))
                                    (result . (5 2))
                                    (result . (3 1))
                                    (result . (9 8))
                                    (result . (4 4)))
            (let ((c5 (client 'call "foo" 8 9))
                  (c4 (client 'call "foo" 1 3))
                  (c3 (client 'call "foo" 2 5))
                  (c2 (client 'call "foo" 1 5))
                  (c1 (client 'call "foo" 6 4))
                  (c6 (client 'call "foo" 4 4)))
              (list
                (client 'wait c1)
                (client 'wait c2)
                (client 'wait c3)
                (client 'wait c4)
                (client 'wait c5)
                (client 'wait c6))))))

  (test-group "integrated multi-co"
    (define (debug co n-co n-th)
      (display (format "~A ~A\n" co n-th) (log-port)))
    (define transport (make-pseudo-transport-multi-co))
    (define client (make-client transport msg-format))
    (define-values (server events stop-server) (make-server transport msg-format debug))
    (thread-start! (make-thread server 'server))

    (test-group "sync-call foo"
      (define-syntax test-sync-call-foo
        (syntax-rules ()
          ((_ client da db a b)
           (test-group (format "sync-call foo ~A ~A" a b)
             (test "result" `(result . (,b ,a)) (client 'sync-call "foo" a b))
             (test "a" a (da))
             (test "b" b (db))))))
      (test-sync-call-foo client foo-a foo-b 4 5)
      (test-sync-call-foo client foo-a foo-b '() '())
      (test-sync-call-foo client foo-a foo-b "ok" "ko")
      (test-sync-call-foo client foo-a foo-b '(1 2 3) '(4 5 6))
      (test-sync-call-foo client foo-a foo-b 'patate 'poulet))

    (test-group "sync-call bar"
      (define-syntax test-sync-call-bar
        (syntax-rules ()
          ((_ client count)
           (test (format "#~A" count) `(result . ,count) (client 'sync-call "bar")))))
      (test-sync-call-bar client 1)
      (test-sync-call-bar client 2)
      (test-sync-call-bar client 3)
      (test-sync-call-bar client 4))

    (test-group "async-call foo" '()
      (test "two concurrent_call" '((result . (4 6)) (result . (5 1)))
            (let ((c1 (client 'call "foo" 6 4))
                  (c2 (client 'call "foo" 1 5)))
              (list
                (client 'wait c1)
                (client 'wait c2))))
      (test "six concurrent_call" '((result . (4 6))
                                    (result . (5 1))
                                    (result . (5 2))
                                    (result . (3 1))
                                    (result . (9 8))
                                    (result . (4 4)))
            (let ((c5 (client 'call "foo" 8 9))
                  (c4 (client 'call "foo" 1 3))
                  (c3 (client 'call "foo" 2 5))
                  (c2 (client 'call "foo" 1 5))
                  (c1 (client 'call "foo" 6 4))
                  (c6 (client 'call "foo" 4 4)))
              (list
                (client 'wait c1)
                (client 'wait c2)
                (client 'wait c3)
                (client 'wait c4)
                (client 'wait c5)
                (client 'wait c6))))
      )
    )
  )

(close-output-port (log-port))
