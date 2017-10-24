# ePort application example

This is an example that tries to illustrate how to start an ePortListener that provides access to multiple
protocol modules. A client need to specify which protocol it wants to access. This means that the actual ePort
link will only use one module, but there is no need to start multiple ePortListeners (on different ports)
in order to provide access to several protocols.

## How to run
In the root folder for the ePort application there is a Makefile, enter that
directory and type in the below command. This will build the source code and
start an erlang shell with the code lodead.
```
make
```

To start the actual example code just type in:
```erlang
exampleMultiple:start().
```

## Files
The example contains three different files that takes care of different things.

| File                  | Function                                             |
|-----------------------|------------------------------------------------------|
| exampleMultiple.erl   | Module to start the example in a convenient way      |
| listProt.erl          | A protocol module that the server provides           |
| randProt.erl          | A protocol module that the client provides           |

## Explanation
The example is started in exampleMultiple.erl, this is an overview of the flow.

1. The server need to be started, this is done with ```ePortListener:start_link([listProt, randProt], Port)```
This setups a listener that will accept connections.

2. Then the clients are started with
```erlang
    {ok, ListPid} =
        ePort:start_link({undefined, listProt}, "localhost", Port),
    {ok, RandPid} =
        ePort:start_link({undefined, randProt}, "localhost", Port),
```
When you run the exampel you can see the clientConnected functions are called in the same manner as when
only specify a single module as the server module.

4. Finally we are calling the append function in the listProt module and the uniform in the randProt module.
The calls are wrapped in an io:format call in order to print out the results from the call.

5. Finally the clients are killed, this is just to illustrate that the clientDisconnected functions on the
server side are called properly.