(module meta-rpc.transport (make-transport-server-fifo
                            <transport-server-fifo>
                            make-transport-client-fifo
                            <transport-client-fifo>

                            make-transport-server-stdio
                            <transport-server-stdio>
                            make-transport-client-stdio
                            <transport-client-stdio>

                            make-transport-server-tcp
                            <transport-server-tcp>
                            make-transport-client-tcp
                            <transport-client-tcp>)
 (import scheme
         chicken.base)
 (import coops)
 (import meta-rpc.interface)
 (include "src/transport/file-io.scm")
 (include "src/transport/tcp.scm")
)
