(import scheme
        chicken.base
        chicken.io)
(import matchable
        srfi-18
        srfi-69
        mailbox)

; public hash table to register methods
(define rpc-methods (make-hash-table))

(define server-min-loop-time (make-parameter 0.05)) ; 50 ms
(define server-max-threads (make-parameter 5))
(define server-max-connections (make-parameter 10))

(define call-method 
  (match-lambda*
    ((('request id method-name params))
     (let ((method (hash-table-ref rpc-methods method-name)))
       (if method 
           (error-as-msg ((err res) (apply method params))
                         (if err
                             (list id method-name err '())
                             (list id method-name '() res)))
           (list id method-name "method not found" '()))))

    ((('notification method-name params))
     (let ((method (hash-table-ref rpc-methods method-name)))
       (if method 
           (log-errors "call-method/notification" (apply method params))
           'no-response)))

    ; response are not supposed to be send to the server
    ((('response id method-name error-obj result))
     (logger "call-method" "Server received an unexpected response: " method-name)
     'no-response)
    ((#f) 'no-response) ; ignore #f, comming from a log-errors block with error
    (any
      (logger "call-method" "Unknown message type received: " any))))

(define (first-ready connections)
  (if (null? connections)
      (values #f '())
      (let ((co (car connections)) (rest (cdr connections)))
        (if (char-ready? (car co))
            (values co rest)
            (first-ready rest)))))

(define (server-worker close-co msg-format input output responses)
  ; thread handling an incoming message
  (let ((response (call-method (log-errors "reading" (msg-format 'rpc-read input)))))
    (if (not (eq? response 'no-response))
        (mailbox-send! responses (cons output response))))
  (if close-co
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
               (msg (cdr mail)))
          (log-errors "writing/response"
                      (msg-format 'rpc-write-response output msg))
          (if close-co
              (close-output-port output))
          (loop (add1 sent))))))

(define (send-events events connections msg-format)
  ; broadcast all pending events to all connected clients
  (unless (or (mailbox-empty? events)
              (null? connections))
    (let loop ((rest connections))
      (unless (null? rest)
          (let* ((output (cdar rest))
                 (event (mailbox-receive! events))
                 (method-name (car event))
                 (args (cdr event)))
            (log-errors "writing/notification"
                        (msg-format 'rpc-write-notification output
                                    (list method-name args)))
            (loop (cdr rest)))))))

(define (make-server transport msg-format)
  (let* ((responses (make-mailbox))
         (close-co (transport 'one-shot?))
         (events (and (not close-co) (make-mailbox))))
    (values
      ; return 2 values: the server main thread ready to be launched and the event mailbox
      (lambda ()
        (let loop ((t-prev (time)) (connections '()) (n-co 0) (n-th 0))
          ; prevent the loop from using 100% CPU for nothing
          (thread-sleep! (+ (server-min-loop-time) t-prev (- (time))))
          (let ((t-now (time)))
            (if events
                ; broadcast event to all connected clients
                (send-events events connections msg-format))
            ; each response ready to send correspond to a finished thread 
            (decr! n-th (send-responses responses close-co msg-format))
            (let-values (((connections n-co) (sweep-connections connections)))
              ; open new threads for incoming messages
              (set! n-th (handle-incoming-msg connections responses close-co n-th msg-format))
              ; handle one of the incoming new connection
              (if (and (transport 'ready) (< n-co (server-max-connections)))
                  (log-errors "new conn"
                              (let-values (((new-in new-out) (transport 'accept)))
                                (loop t-now (cons (cons new-in new-out) connections) (add1 n-co) n-th)))
                  (loop t-now connections n-co n-th))))))
      events)))
