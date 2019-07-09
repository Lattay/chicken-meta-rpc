(import scheme
        chicken.base)
(import matchable
        meta-rpc.interface
        srfi-18
        srfi-69
        mailbox)

; public hash table to register methods
(define *rpc-methods* (make-hash-table))

(define server-min-loop-time (make-parameter 0.05)) ; 50 ms
(define server-max-threads (make-parameter 5))
(define server-max-connections (make-parameter 10))

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
                  code: (get-condition-property err 'app 'code)
                  data: (get-condition-property err 'app 'data)
                  message: (get-condition-property err 'app 'message)))

(define (first-ready connections)
  (if (null? connections)
      (values #f '())
      (let ((co (car connections)) (rest (cdr connections)))
        (if (char-ready? (car co))
            (values co rest)
            (first-ready rest)))))

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
     (cons id (cons method-name (call-method method-name params))))
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

(define (server-worker close-co msg-format input output responses)
  ; thread handling an incoming message
  (let ((result
          (let-values (((err message) (read-message msg-format input)))
                      (if err
                        (list -1 "invalid" err '())
                        (handle-request message)))))
    (unless (null? result)
      (mailbox-send!  responses (cons output result))))
  (when close-co
    (close-input-port input)))

(define (handle-incoming-msg connections responses close-co n-th msg-format)
  ; recursively find the first connection ready to deliver a message
  ; and start a thread to handle it
  (let-values (((co rest) (first-ready connections)))
    (if (and co (< n-th (server-max-threads)))
        (let ((input (car co)) (output (cdr co)))
          (thread-start!
            (lambda ()
              (server-worker close-co msg-format input output responses)))
          (handle-incoming-msg rest responses (add1 n-th) (server-max-threads) msg-format))
        n-th)))

(define (sweep-connections connections)
  ; remove closed connections from the connections list
  (let loop ((rest connections) (kept '()) (n-kept 0))
    (if (null? rest)
        (values kept n-kept)
        (let ((in (caar rest)) (out (cdar rest)))
          (if (and (port-closed? in) (port-closed? out))
              (loop (cdr rest)
                    kept
                    n-kept)
              (loop (cdr rest)
                    (cons (car rest) kept)
                    (add1 n-kept)))))))

(define (send-responses responses close-co msg-format)
  ; send all pending responses to clients
  (let loop ((sent 0))
    (if (mailbox-empty? responses)
        sent
        (let* ((mail (mailbox-receive! responses))
               (output (car mail))
               (message (cdr mail)))
          (log-errors "writing/response"
                      (rpc-write-response msg-format output message))
          (when close-co
              (close-output-port output))
          (loop (add1 sent))))))

(define (send-events events connections msg-format)
  ; broadcast all pending events to all connected clients
  (unless (mailbox-empty? events)
    (let loop ((rest connections))
      (unless (null? rest)
          (let* ((output (cdar rest))
                 (event (mailbox-receive! events))
                 (method-name (car event))
                 (args (cdr event)))
            (log-errors "writing/notification"
                        (rpc-write-notification msg-format output
                                    (list method-name args)))
            (loop (cdr rest)))))))

(define (make-server transport msg-format)
  (assert (and 'is-a-msg-format (message-format? msg-format)))
  (let* ((responses (make-mailbox))
         (close-co (one-shot? transport))
         (events (and (not close-co) (make-mailbox))))
    (values
      ; return 2 values: the server main thread ready to be launched and the event mailbox
      (lambda ()
        (let loop ((t-prev (time-stamp)) (connections '()) (n-co 0) (n-th 0))
          ; prevent the loop from using 100% CPU for nothing
          (let ((t-now (time-stamp)))
            (thread-sleep! (+ (server-min-loop-time) t-prev (- t-now)))
            (when events
              ; broadcast event to all connected clients
              (send-events events connections msg-format))
            ; each response ready to send correspond to a finished thread 
            (decr! n-th (send-responses responses close-co msg-format))
            (let-values (((connections n-co) (sweep-connections connections)))
              ; open new threads for incoming messages
              (set! n-th (handle-incoming-msg connections responses close-co n-th msg-format))
              ; handle one of the incoming new connection
              (if (and (ready? transport) (< n-co (server-max-connections)))
                  (log-errors "new conn"
                              (let-values (((new-in new-out) (accept transport)))
                                (loop t-now (cons (cons new-in new-out) connections) (add1 n-co) n-th)))
                  (loop t-now connections n-co n-th))))))
      events)))
