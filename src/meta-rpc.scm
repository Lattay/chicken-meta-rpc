(module meta-rpc (make-server
                  server-min-loop-time
                  server-max-threads
                  server-max-connections
                  *rpc-methods*

                  make-client
                  client-wait-sleep-time
                  client-wait-timeout
                  client-min-loop-time
                  client-max-connections)

 (import scheme
         chicken.base)
 
 (include "src/main/common.scm")
 (include "src/main/server.scm")
 (include "src/main/client.scm")
)
