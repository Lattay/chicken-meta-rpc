(import scheme
        chicken.base)
(import matchable
        srfi-18
        srfi-69
        mailbox)

(define client-wait-sleep-time (make-parameter 0.05)) ; 50 ms
(define client-min-loop-time (make-parameter 0.05)) ; 50 ms
(define client-max-connections (make-parameter 5))

(define (client-request requests type call-id msg-format method-name args)
  (mailbox-send!
    requests
    (cons call-id 
          (lambda (port)
            (case type
              ((request)
               (rpc-write-request msg-format port
                           (list call-id method-name args)))
              ((notify)
               (rpc-write-notification msg-format port
                           (list method-name args))))))))

(define (client-wait responses call-id)
  ; loop until responses is set in table
  (let ((tmp (hash-table-ref/default responses call-id #f)))
    (if tmp
        (begin
          (hash-table-delete! responses call-id)
          tmp)
        (begin
          (thread-sleep! (client-wait-sleep-time))
          (client-wait responses call-id)))))

(define gen-id
  ; thread safe counter
  (let ((counter 1)
        (lock (make-mutex)))
    (lambda ()
      (let ((tmp #f))
        (mutex-lock! lock)
        (set! tmp counter)
        (incr! counter)
        (mutex-unlock! lock)
        tmp))))

(define (send-requests-one-co transport requests responses connection)
  (unless (mailbox-empty? requests)
    (if (or (not (connection)) (port-closed? (cdr (connection))))
        (let-values (((in out) (connect transport)))
          (connection (cons in out))))
    (let* ((req (mailbox-receive! requests))
           (id (car req))
           (send-to (cdr req)))
      (error-as-msg ((err res) (send-to (cdr (connection))))
                    (if err (hash-table-set! responses id (cons 'error err))))
      (send-requests-one-co transport requests responses connection))))

(define (receive-responses-one-co responses events connection msg-format)
  (when (char-ready? (car (connection)))
    (let ((msg (rpc-read msg-format (car (connection)))))
      (case (car msg)
        ((response)
         (hash-table-set! responses (cadr msg) (cons 'response msg)))
        ((notify)
         (mailbox-send! events msg))
        (else ; should never happen
          '())))
    (receive-responses-one-co responses events connection msg-format)))

(define (send-requests-multi-co transport requests responses connections)
  (unless (or (mailbox-empty? requests)
              (> (client-max-connections) (hash-table-size connections)))
    (let-values (((in out) (connect transport)))
      (let* ((req (mailbox-receive! requests))
             (id (car req))
             (send-to (cdr req)))
        (hash-table-set! connections id (cons in out))
        (error-as-msg ((err res) (send-to out))
                      (if err (hash-table-set! responses id (cons 'error err))))
        (close-output-port out))
      (send-requests-multi-co transport requests responses connections))))

(define (receive-responses-multi-co responses connections msg-format)
  (hash-table-remove!
    connections
    (lambda (id co)
      (if (char-ready? (car co))
          (begin
            (hash-table-set! responses id (cons 'response (rpc-read msg-format (car co))))
            (close-input-port (car co))
            #t)
          #f))))

(define (client-worker transport msg-format requests responses events)
  (let* ((one-co (not (one-shot? transport))) ; one-shot connectionS or multi-shot connection_
         (connection (if one-co (make-parameter #f) #f))
         (connections (if one-co #f (make-hash-table))))
    (let loop ((t-prev (time)))
      (thread-sleep! (+ (client-min-loop-time) (time)))
      (if one-co
          (let ((t-now (time)))
            (send-requests-one-co transport requests responses connection)
            (receive-responses-one-co responses events connection msg-format)
            (loop t-now))
          (let ((t-now (time)))
            (send-requests-multi-co transport requests responses connections)
            (receive-responses-multi-co responses connections msg-format)
            (loop t-now))))))

(define (poll-events! events)
  (let loop ((acc '()))
    (if (mailbox-empty? events)
        acc
        (loop (cons (mailbox-receive! events) acc)))))

(define (make-client transport msg-format)
  (let ((requests (make-mailbox))
        (responses (make-hash-table))
        (events (make-mailbox)))
    (match-lambda*
      (('start) ; start event loop thread
       (thread-start! (lambda () (client-worker transport msg-format requests responses events))))

      (('call method-name args) ; send a request
       (let ((call-id (gen-id)))
         (client-request requests 'request call-id msg-format method-name args)
         call-id))

      (('sync-call method-name args) ; send a request and block until the server respond
       (let ((call-id (gen-id)))
         (client-request requests 'request call-id msg-format method-name args)
         (client-wait responses call-id)))

      (('wait call-id) ; block until the server respond to the request id
       (client-wait responses call-id))

      (('notify method-name args) ; send a notification
       (let ((call-id (gen-id)))
         (client-request requests 'notify call-id msg-format method-name args)
         '()))

      (('poll-events) ; look for server events
       (poll-events! events)))))
