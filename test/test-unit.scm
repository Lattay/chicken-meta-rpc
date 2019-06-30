(import scheme chicken.base)
(import test)

(test-group "unit testing"
(include "test/test-server-unit.scm")
(include "test/test-client-unit.scm")
)
