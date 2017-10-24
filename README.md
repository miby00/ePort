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

Module     %% Name of protocol module, see: example/src/waiter.erl
Port       %% Port to listen to incoming connections on.
AllowedIPs %% List of IP-addresses which the listener accepts incoming connections to (optional)

Example:

{ok, Pid} = ePortListener:start_link(srvProtModule, 19000).
```

### The connecting side
The connecting side starts ePort which connects to a host at a port.

```erlang
ePort:start_link(Module, Host, Port)

Module    %% Name of protocol module for client side, see: example/src/philosopher.erl
Host      %% IP address or computer to connect too
Port      %% Port to connect too

Example:

{ok, Pid} = ePort:start_link(clientProtModule, myserver.com, 19000).
```

When the connecting call from ePort to ePortListener is done, eportListener
calls the function srvProtModule:clientConnected, see src/example/waiter.erl. When
epostListener detects that the socket is down it calls srvProtModule:clientDisconnected.

The server side receives a Pid to use for outgoing communication.

ePort client can only call functions defined in the ePortListeners srvProtModule.
ePort server can only call functions defined in the ePorts clientProtModule.

## Example

There are two examples in the example folder. To run the example just type in 'make' in
root folder of the repository, this starts an erlang shell where you can type:

```erlang
exampleDining:start().
```
or
```erlang
exampleMultiple:start().
```

The first example is a small implementation of the dining philosophers problem using an arbitror.
It seems to be working but could contain some bugs, but it is just to illustrate how ePort works.
If you find any bugs raise an issue please :)
The other example is a simpler example and illustrates how you can update the available protocol
modules provided by the ePortListener side.
There is a separate README in the example directories which describes the examples in more detail.

## Encryption

It is also possible to setup encrypted communication. To do that the ePorts need to be setup
with SSL options. This is done by calling ePort:start_link/4 and with a list of SSL options.
See [Erlang SSL](http://erlang.org/doc/man/ssl.html "Erlang SSL")
