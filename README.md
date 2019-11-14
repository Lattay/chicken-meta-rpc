# About

meta-rpc is intended to be a multi-format [RPC](https://en.wikipedia.org/wiki/Remote_procedure_call)
client and server base for [CHICKEN Scheme](https://call-cc.org).

It is implement an asynchrone server and client, delegating the formating of messages and the transport
to other modules.

Format implementation modules include [json-rpc](https://github.com/lattay/chicken-json-rpc) and
[msgpack-rpc](https://github.com/lattay/chicken-msgpack-rpc).

# Interface

## RPC server app

### Methods

Methods are normal Chicken functions registered in the hash-table
`*rpc-methods*`.
Add entries to this table with the method name as key and function as
value.
Keep in mind that these methods are `apply`d to the array of argument
received from the request.

### Conditions

When raising an error the condition object have to be of the kind `'(exn rpc app)`.
The `app` part must have `code` (an integer), `message` (a string) and `data` properties.
`data` may be `'()` if nothing is relevant.
A simple way to raise such an error is:
```scheme
(signal (condition '(exn location my-method
                         message "some message")
                   '(rpc)
                   '(app code 42
                         message "some message"
                         data ())))
```

Any error that do not follow those rules will be signaled as "Unhandled app error"
without any more informations.

## RPC message format

To implement a new message format one should hinerit from the `coops` class `<rpc-message-format>`
from *meta-rpc.interface*.
It have to implement the six methods to be used by the server and client.

### Read
`(rpc-read msg-format port)`

Read a message from `port`.
It must try to read and return a message of the form:
- `(list 'request id method-name parameters)` for a request
- `(list 'notify id method-name parameters)` for a notification
- `(list 'response id error result)` for a response. No error is represented with `'()`

If the message cannot be interpreted as a valid RPC message, it should raise
condition `'(exn rpc invalid)`.


### Write
`(rpc-write-response msg-format port message)`
`(rpc-write-notification msg-format port message)`
`(rpc-write-request msg-format port message)`

These should send through `port` the valid RPC message according to the standard `msg-format` implement.
For response `message` is of the form `(id error result)`
For requests and notifications `message` is of the form `(id method-name params)`

## RPC transport

To implement a new message format one should subclass `<rpc-transport-server>`
and `<rpc-transport-client>` from *meta-rpc.interface*.
Server class have four methods:
- `(one-shot? serv)` return `#t` if the connection should be used
  for only one exchange, return `#f` if the connection is stable and
  can be used for all exchanges with the client. This mode also allow 
  server to client notifications.
- `(ready? serv)` return `#t` if a new connection is available
- `(accept serv)` return a 2-values, input and output port for
  a new connection
- `(shutdown serv)` perform all actions to close the listener/server side
  transport/connection

# Current project state

Client can proceed to synchronous and asynchronous call and send notifications to the server.
Server can register methods, listen for connections and run actions associated with client requests.

TODO (:100: implemented and tested :o: implemented and not tested :x: not implemented)
- :x: basic transports
    - :100: TCP IPv4
    - :o: File IO
    - :o: Standard IO
- :x: advanced transports
    - :x: Unix port
    - :x: TCP IPv6
- :x: Basic features
    - :100: async requests
    - :100: sync requests
    - :o: notifications
    - :x: server to client notifications
- :x: Benchmark
    - :x: performances
    - :x: resilience
