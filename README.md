meta-rpc is intended to be a multi-format [RPC](https://en.wikipedia.org/wiki/Remote_procedure_call)
client and server implementation for [CHICKEN Scheme](https://call-cc.org)

It is implement an asynchrone server and client, delegating the formating of messages to one of its backend.

Planned backends are json-rpc and msgpack-rpc.
