(module meta-rpc.interface (<message-format>
                            message-format?
                            rpc-write-request
                            rpc-write-response
                            rpc-write-notification
                            rpc-read

                            <transport-server>
                            transport-server?
                            transport-server?
                            one-shot?
                            ready?
                            accept

                            <transport-client>
                            transport-client?
                            transport-client?
                            connect)

  (import scheme chicken.base)
  (import coops)
  (include "src/interface/message-format.scm")
  (include "src/interface/transport.scm")
)
