(import scheme chicken.base)
(import test)

(test-group "unit testing"
(include "test/server-unit.scm")
(include "test/client-unit.scm")
)
