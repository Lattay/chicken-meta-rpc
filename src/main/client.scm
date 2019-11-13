(import scheme
        chicken.base
        chicken.format
        chicken.condition)
(import matchable
        meta-rpc.interface
        srfi-18
        srfi-69
        mailbox)

(define client-wait-sleep-time (make-parameter 0.05)) ; 50 ms
(define client-min-loop-time (make-parameter 0.05)) ; 50 ms
(define client-max-connections (make-parameter 5))
(define client-wait-timeout (make-parameter 600))

(define (make-multi-thread-parameter value)
  (let ((val value))
        (lambda arg
          (when (not (null? arg))
              (set! val (car arg)))
          val)))


(define-syntax error-as-msg
  (syntax-rules ()
    ((_ ((err res) expr) body ...)
     (let ((res #f) (err #f))
       (condition-case (set! res expr)
                       (e (exn arity) (set! err 
                                        (string-append 
                                          "Wrong number of argument: "
                                          (get-exn-msg e))))
                       (e (exn type) (set! err
                                       (string-append
                                         "Type error: "
                                         (get-exn-msg e))))
                       (e (exn) (set! err
                                  (string-append
                                    "Error: "
                                    (get-exn-msg e)))))
       body ...))))

(define (make-timeout-error id t)
  (let ((message (format "timeout error waiting for ~A for ~A s" id t)))
    (condition
      `(exn location rpc-client message ,message)
      '(rpc)
      '(client)
      `(timeout id ,id))))

; Responses
(define (get-response table id)
  (hash-table-ref/default table id #f))

(define (unset-response! table id)
  (hash-table-delete! table id))

(define (set-response! table id #!key (error '()) (result '()))
  (hash-table-set!
    table id
    (cons
      (if (hash-table? error) (hash-table->alist error) error)
      result)))

(define (client-request requests type call-id msg-format method-name args)
  ; add a request/notification to the queue 
  (mailbox-send!
    requests
    (cons call-id 
          (lambda (port) ; send-to function
            (case type
              ((request)
               (rpc-write-request msg-format port
                           (list call-id method-name args)))
              ((notify)
               (rpc-write-notification msg-format port
                           (list method-name args)))
              (else 
                (error (format "noting like this ~A" type))
                #f))))))

(define (client-wait responses call-id)
  ; loop until responses is set in table
  (let loop ((start-waiting (time-stamp)))
    (let ((tmp (get-response responses call-id)))
      (if tmp
          (let ((err (car tmp)) (result (cdr tmp)))
            (unset-response! responses call-id)
            (if err
                (cons 'error err)
                (cons 'result result)))
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
                    (when err (set-response! responses id error: err)))
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
                      (when err (set-response! responses id error: err)))
        (close-output-port out))
      (send-requests-multi-co transport requests responses connections))))

(define (read-msg msg-format port)
  (condition-case (rpc-read msg-format port)
    ((exn rpc invalid)
     '(error "invalid message"))
    ((exn i/o)
     '(error "i/o failure"))
    (e (exn)
       (signal e))))

(define (receive-responses-one-co responses events connection msg-format)
  (when (char-ready? (car (connection)))
    (let ((msg (read-msg msg-format (car (connection)))))
      (case (car msg)
        ((response)
         (set-response! responses (cadr msg)
                        error: (cadddr msg)
                        result: (car (cddddr msg))))
        ((notify)
         (mailbox-send! events msg))
        ((error)
         ; (mailbox-send! events msg))
         (error msg))
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
               (set-response! responses id error: err result: result))
              ((error err)
               (set-response! responses id error: err))
              (()
               (set-response! responses id error: 'invalid-response))
              (any
                (signal
                  (condition
                    `(exn location receive-response message ,any)))))
            (close-input-port (car co))
            #t)
          #f))))

(define (client-worker transport msg-format requests responses events stop)
  (let* ((one-co (not (one-shot? transport))) ; one-shot connectionS or multi-shot connection_
         (connection (if one-co (make-multi-thread-parameter #f) #f))
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
         (stop-client (make-multi-thread-parameter #f))
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
