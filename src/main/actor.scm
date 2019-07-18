(import scheme
        chicken.base
        chicken.format)
(import coops
        srfi-18
        mailbox)
(include "src/main/common.scm")

(define-class <actor> ()
  ((private-mailbox (make-mailbox)) (private-continue #t)))

(define-method (work (self <actor>) #!key (timeout #f))
  (let ((start (time-stamp)))
    (let loop ()
      (let ((m (mailbox-receive! (slot-value self 'private-mailbox)
                                 (if timeout
                                     (min 1 (- (time-stamp) start timeout))
                                     1)
                                 #f)))
        (if m
            (handle self (car m) (cdr m))))
      (if (and
            (slot-value self 'private-continue)
            (if timeout (> timeout (- (time-stamp) start)) #t))
          (loop)))))

(define-method (handle (self <actor>) msg data)
  (case msg
    ((stop)
     (set! (slot-value self 'private-continue) #f))
    (else
      (signal (condition `(exn location ,(string->symbol (format "~A:handle" (class-name (class-of self))))
                               message ,(format "Unexpected message received: ~A" msg))
                         `(actor unexpected-message ,msg))))))

(define-method (send (self <actor>) msg data)
  (mailbox-send! (slot-value self 'private-mailbox) (cons msg data)))
