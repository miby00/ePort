# ePort application example

This is an example that implements the dining philosopher problem using an
arbitror as the solution. There are better solutions, but the arbitror solution
works well to illustrate how the ePort application works.

## How to run
In the root folder for the ePort application there is a Makefile, enter that
directory and type in the below command. This will build the source code and
start an erlang shell with the code lodead.
```
make
```

To start the actual example code just type in:
```erlang
example:start().
```

## Files
The example contains five different files that takes care of different things.

| File                  | Function                                                                           |
|-----------------------|------------------------------------------------------------------------------------|
| example.erl           | "Wrapper" module to start the example in a convenient way                          |
| waiter.erl            | The protocol module that the server provides                                       |
| waiterServer.erl      | A gen server that keeps a state and provides values for the server protocol module |
| philosopher.erl       | The protocol module that the client provides                                       |
| philosopherServer.erl | A gen server that keeps a state and provides values for the client protocol module |