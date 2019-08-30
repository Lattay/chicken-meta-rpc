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

To implement a new message format one should subclass `<rpc-message-format>` from 
*meta-rpc.interface*.
It have to implement the following methods:
- `(rpc-read msg-format port)`
- `(rpc-write-response msg-format port message)`
- `(rpc-write-notification msg-format port message)`
- `(rpc-write-request msg-format port message)`

The `rpc-read` method should raise condition `'(exn rpc invalid)` when
the message cannot be interpreted as a valid RPC message.

## RPC transport

To implement a new message format one should subclass `<rpc-transport-server>`
and `<rpc-transport-client>` from *meta-rpc.interface*.
Server class have three methods:
- `(one-shot? serv)` return `#t` if the connection should be used
  for only one exchange, return `#f` if the connection is stable and
  can be used for all exchanges with the client. This mode also allow 
  server to client notifications.
- `(ready? serv)` return `#t` if a new connection is available
- `(accept serv)` return a 2-values, input and output port for
  a new connection

# Current project state

The basic features of the client and the server are implemented and tested.
Client can proceed to synchronous and asynchronous call and send notifications to the server.
Server can register methods, listen for connections and run actions associated with client requests.

"Events" are the way to initiate server-to-client communication. A server can broadcast an event to all
its client when the connection is in persistent mode. This feature is yet to be implemented.
