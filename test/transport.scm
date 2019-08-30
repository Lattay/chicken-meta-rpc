(import scheme
        chicken.base
        chicken.tcp
        chicken.io
        chicken.format)
(import test srfi-69 srfi-18 coops)

(load "meta-rpc.interface.so")
(import meta-rpc.interface)

(load "meta-rpc.transport.so")
(import meta-rpc.transport)

(load "test/pseudo-format.scm")
(define msg-format (make-pseudo-format))

(tcp-read-timeout 500)
(tcp-write-timeout 500)

(test-group "transport"
  (test-group "tcp"
    (test-group "server"
      (define tr (make-transport-server-tcp "localhost" 4567))
      (test "not ready" #f (ready? tr))

      (define-values (in-cli out-cli) (tcp-connect "localhost" 4567))
      (test "ready" #t (ready? tr))
      (define-values (in-serv out-serv) (accept tr))
      (test "no longer ready" #f (ready? tr))
      (test "server-to-client-write" '(truc "foo" 56)
            (begin
              (write '(truc "foo" 56) out-serv)
              (read in-cli)))
      (test "server-to-client formatted" '(notification "foo" (1 2 3))
            (begin
              (rpc-write-notification msg-format out-serv '("foo" (1 2 3)))
              (rpc-read msg-format in-cli)))

      (test "client-to-server-write" '(truc "foo" 56)
            (begin
              (write '(truc "foo" 56) out-cli)
              (read in-serv)))
      (test "client-to-server formatted" '(request 1 "foo" (1 2 3))
            (begin
              (rpc-write-request msg-format out-serv '(1 "foo" (1 2 3)))
              (rpc-read msg-format in-cli)))
      (close-output-port out-cli)
      (close-output-port out-serv)
      (close-input-port in-cli)
      (close-input-port in-serv)
      (shutdown tr)
      )

    (test-group "client"
      (define listener (tcp-listen 4568))
      (define tr (make-transport-client-tcp "localhost" 4568))
      (define-values (in-cli out-cli) (connect tr))
      (test "ready" #t (tcp-accept-ready? listener))
      (define-values (in-serv out-serv) (tcp-accept listener))
      (test "server-to-client-write" '(truc "foo" 56)
            (begin
              (write '(truc "foo" 56) out-serv)
              (read in-cli)))
      (test "server-to-client formatted" '(notification "foo" (1 2 3))
            (begin
              (rpc-write-notification msg-format out-serv '("foo" (1 2 3)))
              (rpc-read msg-format in-cli)))

      (test "client-to-server-write" '(truc "foo" 56)
            (begin
              (write '(truc "foo" 56) out-cli)
              (read in-serv)))
      (test "client-to-server formatted" '(request 1 "foo" (1 2 3))
            (begin
              (rpc-write-request msg-format out-serv '(1 "foo" (1 2 3)))
              (rpc-read msg-format in-cli)))
      (tcp-close listener)
      )
    )

  ; (test-group "fifo"
  ;   (test-group "server"
  ;     (define tr (make-transport-server-tcp "localhost" 4567))
  ;     (test "not ready" #f (ready? tr))
  ; 
  ;     (define-values (in-cli out-cli) (tcp-connect "localhost" 4567))
  ;     (test "ready" #t (ready? tr))
  ;     (define-values (in-serv out-serv) (accept tr))
  ;     (test "no longer ready" #f (ready? tr))
  ;     (test "server-to-client-write" '(truc "foo" 56)
  ;           (begin
  ;             (write '(truc "foo" 56) out-serv)
  ;             (read in-cli)))
  ;     (test "server-to-client formatted" '(notification "foo" (1 2 3))
  ;           (begin
  ;             (rpc-write-notification msg-format out-serv '("foo" (1 2 3)))
  ;             (rpc-read msg-format in-cli)))
  ; 
  ;     (test "client-to-server-write" '(truc "foo" 56)
  ;           (begin
  ;             (write '(truc "foo" 56) out-cli)
  ;             (read in-serv)))
  ;     (test "client-to-server formatted" '(request 1 "foo" (1 2 3))
  ;           (begin
  ;             (rpc-write-request msg-format out-serv '(1 "foo" (1 2 3)))
  ;             (rpc-read msg-format in-cli)))))
  )
