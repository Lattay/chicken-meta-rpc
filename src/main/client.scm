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
  (if (not (hash-table-exists? table id))
      #f
      (hash-table-ref table id)))

(define (unset-response! table id)
  (hash-table-delete! table id))

(define (set-response! table id #!key (error '()) (result '()))
  (hash-table-set!
    table id
    (cons
      (if (hash-table? error) (hash-table->alist error) error)
      result)))

(define (send-request-one-co transport id request on-error connection)
  (when (or (not (connection)) (port-closed? (cdr (connection))))
    (let-values (((in out) (connect transport)))
      (connection (cons in out))))
  (error-as-msg ((err _) (request (cdr (connection))))
                (when err (on-error err))))

(define (send-request-multi-co transport id request on-error connections)
  (if (< (client-max-connections) (hash-table-size connections))
      (error "Too many opened connections."))
  (let-values (((in out) (connect transport)))
    (hash-table-set! connections id (cons in out))
    (error-as-msg ((err _) (request out))
                  (when err (on-error err)))
    (close-output-port out)))

(define (client-request type call-id msg-format transport connection.s responses method-name args)
  ; add a request/notification to the queue 
  (let ((one-co (not (one-shot? transport)))
        (req (lambda (port) ; request function
               (case type
                 ((request)
                  (rpc-write-request msg-format port
                                     (list call-id method-name args)))
                 ((notify)
                  (rpc-write-notification msg-format port
                                          (list method-name args)))
                 (else 
                   (error (format "noting like this ~A" type))
                   #f))))
        (on-error (lambda (err)
                    (set-response! responses call-id error: err))))
    ((if one-co send-request-one-co send-request-multi-co)
     transport call-id req on-error connection.s)))

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
                        error: (caddr msg)
                        result: (car (cdddr msg))))
        ((notify)
         (mailbox-send! events msg))
        ((error)
         (mailbox-send! events msg)))
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
              ((response id err result)
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

(define (client-waiter transport msg-format one-co connection.s requests responses events)
  (lambda (call-id)
    (let wait-for-id ((timeout (+ (client-wait-timeout) (time-stamp))))
      (if (>= (time-stamp) timeout)
          '(timeout)
          (begin
            ((if one-co receive-responses-one-co receive-responses-multi-co)
             responses events connection.s msg-format)
            (let ((resp (get-response responses call-id)))
              (if resp
                  (let ((err (car resp)) (result (cdr resp)))
                    (unset-response! responses call-id)
                    (if (null? err)
                        (cons 'result result)
                        (cons 'error err)))
                  (wait-for-id timeout))))))))

(define (poll-events! events)
  (let loop ((acc '()))
    (if (mailbox-empty? events)
        acc
        (loop (cons (mailbox-receive! events) acc)))))

(define (make-client transport msg-format #!optional (autostart #t))
  (let* ((requests (make-mailbox))
         (responses (make-hash-table))
         (events (make-mailbox))
         (one-co (not (one-shot? transport)))
         (connection.s (if one-co (make-parameter #f) (make-hash-table)))
         (wait (client-waiter transport msg-format one-co connection.s
                                requests responses events)))
    (match-lambda*
      (('call method-name . args) ; send a request
       (let ((call-id (gen-id)))
         (client-request 'request call-id msg-format transport connection.s responses method-name args)
         call-id))

      (('sync-call method-name . args) ; send a request and block until the server respond
       (let ((call-id (gen-id)))
         (client-request 'request call-id msg-format transport connection.s responses method-name args)
         (wait call-id)))

      (('wait call-id) ; block until the server respond to the request id
       (wait call-id))

      (('notify method-name . args) ; send a notification
       (let ((call-id (gen-id)))
         (client-request 'notify call-id msg-format transport connection.s responses method-name args)
         '()))

      (('poll-events) ; look for server events
       (poll-events! events)))))
