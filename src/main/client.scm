(import scheme
        chicken.base
        chicken.format
        chicken.condition)
(import srfi-18
        srfi-69
        mailbox)

(define client-wait-sleep-time (make-parameter 0.05)) ; 50 ms
(define client-min-loop-time (make-parameter 0.05)) ; 50 ms
(define client-max-connections (make-parameter 5))
(define client-wait-timeout (make-parameter 600))

(define (make-timeout-error id t)
  (let ((message (format "timeout error waiting for ~A for ~A s" id t)))
    (condition
      `(exn location rpc-client message ,message)
      '(rpc)
      '(client)
      `(timeout id ,id))))

(define (client-request requests type call-id msg-format method-name args)
  ; add a request/notification to the queue 
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
                           (list method-name args)))
              (else #f))))))

(define (client-wait responses call-id)
  ; loop until responses is set in table
  (let loop ((start-waiting (time-stamp)))
    (let ((tmp (hash-table-ref/default responses call-id #f)))
      (if tmp
        (begin
          (hash-table-delete! responses call-id)
          tmp)
        (begin
          (thread-sleep! (client-wait-sleep-time))
          (let ((waited  (- (time-stamp) start-waiting)))
            (if (> waited (client-wait-timeout))
              (signal (make-timeout-error call-id waited))
              (loop start-waiting))))))))

(define gen-id
  ; thread safe counter
  (let ((counter 1)
        (lock (make-mutex)))
    (lambda ()
      (let ((tmp #f))
        (mutex-lock! lock)
        (set! tmp counter)
        (set! counter (add1 counter))
        (mutex-unlock! lock)
        tmp))))

(define (send-requests-one-co transport requests responses connection)
  (unless (mailbox-empty? requests)
    (when (or (not (connection)) (port-closed? (cdr (connection))))
        (let-values (((in out) (connect transport)))
          (connection (cons in out))))
    (let* ((req (mailbox-receive! requests))
           (id (car req))
           (send-to (cdr req)))
      (error-as-msg ((err _) (send-to (cdr (connection))))
                    (when err (hash-table-set! responses id (cons 'error err))))
      (send-requests-one-co transport requests responses connection))))

(define (send-requests-multi-co transport requests responses connections)
  (unless (or (mailbox-empty? requests)
              (< (client-max-connections) (hash-table-size connections)))
    (let-values (((in out) (connect transport)))
      (let* ((req (mailbox-receive! requests))
             (id (car req))
             (send-to (cdr req)))
        (hash-table-set! connections id (cons in out))
        (error-as-msg ((err _) (send-to out))
                      (when err (hash-table-set! responses id (cons 'error err))))
        (close-output-port out))
      (send-requests-multi-co transport requests responses connections))))

(define (read-msg msg-format port)
  (condition-case (rpc-read msg-format port)
    ((exn rpc invalid)
     '(error "invalid message"))
    ((exn i/o)
     '(error "i/o failure"))
    (e (exn)
       `(error ,e))))

(define (receive-responses-one-co responses events connection msg-format)
  (when (char-ready? (car (connection)))
    (let ((msg (read-msg msg-format (car (connection)))))
      (case (car msg)
        ((response)
         (hash-table-set! responses (cadr msg) (cdddr msg)))
        ((notify)
         (mailbox-send! events msg))
        ((error)
         (mailbox-send! events msg))
        (else ; should not happend
          (let ((m (format "fuck this ~A!" msg)))
            (signal
              (condition
                `(exn location receive-response message ,m)))))))
    (receive-responses-one-co responses events connection msg-format)))

(define (receive-responses-multi-co responses connections msg-format)
  (hash-table-remove!
    connections
    (lambda (id co)
      (if (char-ready? (car co))
          (let ((msg (read-msg msg-format (car co))))
            (match msg
              ((response id method err result)
               (hash-table-set! responses id (list err result)))
              ((error err)
               (hash-table-set! responses id (list err '())))
              (any
                (signal
                  (condition
                    `(exn location receive-response message ,any)))))
            (close-input-port (car co))
            #t)
          #f))))

(define (client-worker transport msg-format requests responses events stop)
  (let* ((one-co (not (one-shot? transport))) ; one-shot connectionS or multi-shot connection_
         (connection (if one-co (make-parameter #f) #f))
         (connections (if one-co #f (make-hash-table))))
    (let loop ((t-prev (time-stamp)))
      (if (stop)
          '()
          (let ((t-now (time-stamp)))
            (thread-sleep! (- (+ (client-min-loop-time) t-prev) t-now))
            (if one-co
                (begin
                  (send-requests-one-co transport requests responses connection)
                  (receive-responses-one-co responses events connection msg-format)
                  (loop t-now))
                (begin
                  (send-requests-multi-co transport requests responses connections)
                  (receive-responses-multi-co responses connections msg-format)
                  (loop t-now))))))))

(define (poll-events! events)
  (let loop ((acc '()))
    (if (mailbox-empty? events)
        acc
        (loop (cons (mailbox-receive! events) acc)))))

(define (make-client transport msg-format #!optional (autostart #t))
  (let* ((requests (make-mailbox))
         (responses (make-hash-table))
         (events (make-mailbox))
         (stop-client (make-parameter #f))
         (start-client (make-thread
                         (lambda ()
                           (client-worker transport msg-format requests
                                          responses events stop-client))
                         'client-worker)))
    (if autostart
        (thread-start! start-client))
    (match-lambda*
      (('start) ; start event loop thread
       (if (not autostart)
           (thread-start! start-client)))

      (('stop)
       (stop-client #t))

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
