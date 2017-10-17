# ePort application

ePort is a library to make easy but restricted rpc:calls between erlang components
running on diffrent nodes.

The normal rpc module uses node communication to reply with its result which is
not possible to use if the two components that should be communicating is run by
diffrent users.

## How to connect

### The listener side
The listener side starts upp an ePortListener which listens for incoming
connections (default port 19000)

```erlang
ePortListener:start_link(Module, Port, AllowedIPs)

Module     = Name of protocol module, see: example/src/waiter.erl
Port       = Port to listen to incoming connections on.
AllowedIPs = List of IP-addresses which the listener accepts incoming
                 connections to (optional)

Example:

{ok, Pid} = ePortListener:start_link(srvProtModule, 19000).
```

### The connecting side
The connecting side starts ePort which connects to a host at a port.

```erlang
ePort:start_link(Module, Host, Port)

Module     = Name of protocol module for client side, see: example/src/philosopher.erl
Host       = IP address or computer to connect too
Port       = Port to connect too

Example:

{ok, Pid} = ePort:start_link(clientProtModule, myserver.com, 19000).
```

When the connecting call from ePort to ePortListener is done, eportListener
calls the function srvProtModule:clientConnected, see src/example/waiter.erl. When
epostListener detects that the socket is down it calls srvProtModule:clientDisconnected.

The server side receives a Pid to use for outgoing communication.

ePort client can only call functions defined in the ePortListeners srvProtModule.
ePort server can only call functions defined in the ePorts clientProtModule.

### Example

There are two examples in the example folder. To run the example just type in 'make' in
root folder of the repository. Then type in either

```erlang
exampleDining:start().

or

exampleMultiple:start().
```

The example is a small implementation of the dining philosophers problem using an arbitror.
It seems to be working but could contain some bugs, but it is just to illustrate how ePort works.
If you find any bugs raise an issue please :)
There is a separate README in the example directory which describes the example in more detail.

### Last but least...

Apart from doing call:s using ePort you can also do cast:s in the same way
as described above.

It is also possible to setup encrypted communication, see ePort for an example of
how to do that.
