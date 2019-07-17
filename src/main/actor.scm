(import scheme
        chicken.base
        chicken.format)
(import coops
        srfi-18
        mailbox)

(define-class <actor> ()
  ((private-mailbox (make-mailbox)) (private-continue #t)))

(define-method (work (self <actor>))
  (let loop ()
    (let ((m (mailbox-receive! (slot-value self 'private-mailbox) 1 #f)))
      (if m
          (handle self (car m) (cdr m))))
    (if (slot-value self 'private-continue)
        (loop))))

(define-method (handle (self <actor>) msg data)
  (case msg
    ((stop)
     (set! (slot-value self 'private-continue) #f))
    (else
      (signal (condition `(exn location <actor>:handle
                               message ,(format "Unexpected message received: ~A" msg))
                         `(actor unexpected-message ,msg))))))

(define-method (send (self <actor>) msg data)
  (mailbox-send! (slot-value self 'private-mailbox) (cons msg data)))
