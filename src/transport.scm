(module meta-rpc.transport (make-fifo-transport
                            make-stdio-transport
                            make-tcp-client-transport
                            make-tcp-server-transport)
 (import chicken.base)
 (include "src/transport/file-io.scm")
 (include "src/transport/tcp.scm")
)
