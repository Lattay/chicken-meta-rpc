(import scheme
        chicken.base
        chicken.condition)
(import matchable
        meta-rpc.interface
        srfi-18
        srfi-69
        queues)

(include "src/main/actor.scm")

; public hash table to register methods
(define *rpc-methods* (make-hash-table))

(define server-min-loop-time (make-parameter 0.05)) ; 50 ms
(define server-max-threads (make-parameter 5))
(define server-max-connections (make-parameter 10))

;;;;;;;;;;;;;;;;;;;; Records ;;;;;;;;;;;;;;;;;;;;

(define-record-type conn
  (make-conn in out)
  conn?
  (in conn-in)
  (out conn-out))

(define (close-connection co)
  (close-input-port (conn-in co))
  (close-output-port (conn-out co)))

;;;;;;;;;;;;;;;;;;;; Tools ;;;;;;;;;;;;;;;;;;;;
(define (make-rpc-error type #!key (message "") (data '()) (code #f))
  (let ((err (make-hash-table)))
    (hash-table-set! err "data" '())
    (case type
      ((parse-error) 
       (hash-table-set! err "message" "Parse error")
       (hash-table-set! err "code" -32700))
      ((invalid-request)
       (hash-table-set! err "message" "Invalid request")
       (hash-table-set! err "code" -32600))
      ((method-not-found)
       (hash-table-set! err "message" "Method not found")
       (hash-table-set! err "code" -32601))
      ((invalid-params)
       (hash-table-set! err "message" "Invalid parameter")
       (hash-table-set! err "code" -32602))
      ((internal-error)
       (hash-table-set! err "message" "Internal error")
       (hash-table-set! err "code" -32603))
      ((app-error)
       (hash-table-set! err "message" "Unhandled app error")
       (hash-table-set! err "code" -5000))
      ((server-error)
       (hash-table-set! err "data" data)
       (case code
         ((-32001)
          (hash-table-set! err "code" -32001)
          (hash-table-set! err "message" "I/O error"))
         (else
           (hash-table-set! err "message" "Server error")
           (hash-table-set! err "code" -32000))))
      (else
        (hash-table-set! err "code" code)
        (hash-table-set! err "message" message)
        (hash-table-set! err "data" data)))
    err))

(define (make-app-error err)
  (make-rpc-error '()
                  code: (get-condition-property err 'app 'code '())
                  data: (get-condition-property err 'app 'data '())
                  message: (get-condition-property err 'app 'message '())))

(define (call-method name params)
  ; call method and handle errors
  (if (not (hash-table-exists? *rpc-methods* name))
      (list (make-rpc-error 'method-not-found) '())
      (condition-case
        (list '() (apply (hash-table-ref *rpc-methods* name) params))

        (e (exn rpc app)
           (list (make-app-error e) '()))
        ((exn)
         (list (make-rpc-error 'app-error) '())))))

(define handle-request
  ; interpret request message and handle error
  ; msg-format is supposed to be reliable thought, so this error
  ; should not happen
  (match-lambda*
    ((('request id method-name params))
     `(,id ,method-name . ,(call-method method-name params)))
    ((('notification method-name params))
     (call-method method-name params)
     '())
    (any
      (list -1 "invalid" (make-rpc-error 'invalid-request) '()))))

(define (read-message msg-format input)
  ; parse request message and handle error
  (condition-case
    (values #f (rpc-read msg-format input))
    ((exn rpc invalid)
     (values (make-rpc-error 'parse-error) #f))
    ((exn i/o)
     (values (make-rpc-error 'server-error code: -32001) #f))
    (e (exn)
       (values (make-rpc-error 'server-error data: e) #f))))

;;;;;;;;;;;;;;;;;;;; Actors ;;;;;;;;;;;;;;;;;;;;

; Connection store
(define-class <connection-store> (<actor>)
  ((connections (make-queue))
   (max (server-max-connections))
   scheduler))

(define-method (handle (self <connection-store>) msg data)
  (case msg
    ((store-connection)
     (let ((co data))
       (assert (and 'store-connection (conn? co)))
       (queue-add! (slot-value self 'connections)  co)))

    ((ask-for-space)
     (let ((who data))
       (if (> (slot-value self 'max) (queue-length (slot-value self 'connections)))
           (send who 'new-connection '())
           (begin
             (send self 'clean-connections '())
             (send self 'ask-for-space who)))))

    ((check-connections)
     (let loop ((rest (queue-length (slot-value self 'connections))))
       (if (zero? rest)
           0
           (let ((co (queue-remove! (slot-value self 'connections))))
             (if (char-ready? (conn-in co))
                 (send (slot-value self 'scheduler) 'new-message co)
                 (begin
                   (queue-add! (slot-value self 'connections) co)
                   (loop (sub1 rest))))))))

    ((clean-connections)
     (let loop ((rest (queue-length (slot-value self 'connections))))
       (unless (zero? rest)
         (let ((co (queue-remove! (slot-value self 'connections))))
           (unless (port-closed? (conn-out co))
             (queue-add! (slot-value self 'connections) co))
           (loop (sub1 rest))))))
    (else
      (call-next-method))))

(define (make-connection-store scheduler)
  (make <connection-store> 'scheduler scheduler))

; Scheduler
(define-class <scheduler> (<actor>)
  ((workers (make-vector (server-max-threads) #f))
   format
   one-shot
   connection-store))

(define (make-scheduler msg-format one-shot)
  (make <scheduler> 'format msg-format 'one-shot one-shot))

(define-method (set-connection-store! (self <scheduler>) (cst <connection-store>))
  (set! (slot-value self 'connection-store) cst))

(define-method (handle (self <scheduler>) msg data)
  (case msg
    ((new-message)
     (handle-new-message self data))
    ((new-event)
     '())
    ((task-error)
     (let ((co (car data)) (id (cadr data)) (err (cddr data)))
       ((car (vector-ref (slot-value self 'workers) id)) #f)  ; not busy
       (if (slot-value self 'one-shot)
           (close-connection co)
           (send (slot-value self 'connection-store) 'store-connection co))))
    ((task-done)
     (let ((co (car data)) (id (cadr data)) (res (cddr data)))
       ((car (vector-ref (slot-value self 'workers) id)) #f)  ; not busy
       (if (slot-value self 'one-shot)
           (close-connection co)
           (send (slot-value self 'connection-store) 'store-connection co))))
    (else
      (call-next-method))))

(define-method (handle-new-message (self <scheduler>) co)
  (let* ((workers (slot-value self 'workers))
         (end (vector-length workers)))
    (let loop ((i 0))
      (if (> end i)
          (send self 'new-message co)
          (let ((this-wk (vector-ref workers i)))
            (if this-wk
                (let ((busy (car this-wk)) (wk (cdr this-wk)))
                  (if (busy)
                      (loop (add1 i))
                      (begin
                        (busy #t)
                        (send wk 'new-message (cons i co)))))
                (let ((wk (make-worker self (slot-value self 'format))))
                  (vector-set! workers i (cons (make-parameter #t) wk))
                  (send wk 'new-message (cons i co)))))))))


; Workers
(define-class <worker> (<actor>)
  (parent format))

(define (make-worker parent msg-format)
  (let ((wk (make <worker> 'parent parent 'format msg-format)))
    (thread-start! (lambda () (work wk)))
    wk))

(define-method (handle (self <worker>) msg data)
  (case msg
    ((new-message)
     (let ((id (car data)) (co (cdr data)))
       (handle-new-message self id co)))
    ((send-event-to)
     (handle-send-event self (car data) (cdr data)))
    (else
      (call-next-method))))

(define-method (handle-new-message (self <worker>) id co)
  (let-values (((err msg) (read-message (slot-value self 'format) (conn-in co))))
    (if err
        (send (slot-value self 'parent) 'task-error `(,id ,co . ,err))
        (let ((res (handle-request msg)))
          (unless (null? res)
            (log-errors "writing/response"
                        (rpc-write-response (slot-value self 'format ) (conn-out co) res)))
          (send (slot-value self 'parent) 'task-done `(,id ,co . #t))))))

(define-method (handle-send-event (self <worker>) dest event)
  '())

; Transport listener

(define-class <listener> (<actor>)
  (transport connection-store))

(define (make-listener transport cs)
  (make <listener> 'transport transport 'connection-store cs))

(define-method (handle (self <listener>) msg data)
  (case msg
    ((check-transport)
     (if (ready? (slot-value self 'transport))
         (send (slot-value self 'connection-store) 'ask-for-space self)))
    ((new-connection)
     (let-values (((in out) (accept (slot-value self 'transport))))
       (send (slot-value self 'connection-store) 'store-connection (make-conn in out))))
    (else
      (call-next-method))))

; Master

(define-class <master> (<actor>)
  (sch cst lis))

(define (make-master sch cst lis)
  (make <master> 'sch sch 'cst cst 'lis lis))

(define-method (handle (self <master>) msg data)
  (case msg
    ((kill-all)
     (send (slot-value self 'sch) 'kill-all '())
     (send (slot-value self 'cst) 'stop '())
     (send (slot-value self 'lis) 'stop '()))
    ((run)
     (let ((i data))
       (send (slot-value self 'lis) 'check-transport '())
       (send (slot-value self 'cst) 'check-connections '())
       (send self 'run (add1 i))
       (thread-sleep! (server-min-loop-time))))
    ((new-event)
     '())
    (else
      (call-next-method))))


;;;;;;;;;;;;;;;;;;;; Main entry point ;;;;;;;;;;;;;;;;;;;;
(define (make-server transport msg-format #!optional (debug (lambda args '())))
  (let* ((one-shot (one-shot? transport))
         (sch (make-scheduler msg-format one-shot))
         (cst (make-connection-store sch))
         (lis (make-listener transport cst))
         (master (make-master sch cst lis))
         (started (make-parameter #f)))
    (set-connection-store! sch cst)
    (values
      (lambda (#!key (timeout #f) (async #f))
        (unless (started)
          (thread-start! (lambda ()
                           (let ((q1 (* 1/5 (server-min-loop-time)))
                                 (q2 (* 1/5 (server-min-loop-time))))
                             (let loop ()
                               (work lis q1)
                               (work cst q2)
                               (work sch q2)
                               (loop)))))
          (send master 'run 0)
          (started #t))
        (if timeout
          (work master timeout)
          (if async
              (thread-start! (lambda () (work master)))
              (work master))))
      (lambda (event)
        (send master 'new-event event)))))
