(import scheme chicken.base chicken.format)
(import test srfi-69 srfi-18)

(load "test/test-pseudo-transport.scm")
(load "test/test-pseudo-format.scm")

(include "src/main/common.scm")
(include "src/main/server.scm")
(include "src/main/client.scm")

