(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18)

(load "test/test-pseudo-transport.scm")
(load "test/test-pseudo-format.scm")

(include "src/main/common.scm")
(include "src/main/server.scm")
(include "src/main/client.scm")

(define transport (make-pseudo-transport #f))
(define msg-format (make-pseudo-format))
(define client (make-client transport msg-format))

(define foo-a (make-parameter #f))
(define foo-b (make-parameter #f))
(define bar (make-parameter 0))
(define done (make-parameter #f))

(test-group "full-client"

  (set! transport (make-pseudo-transport #f)))
