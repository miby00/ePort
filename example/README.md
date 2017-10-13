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
* example.erl
* waiter.erl
* waiterServer.erl
* philosopher.erl
* philosopherServer.erl