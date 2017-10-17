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
exampleDining:start().
```

## Files
The example contains five different files that takes care of different things.

| File                  | Function                                                                           |
|-----------------------|------------------------------------------------------------------------------------|
| exampleDining.erl     | Module to start the example in a convenient way                                    |
| waiter.erl            | The protocol module that the server provides                                       |
| waiterServer.erl      | A gen server that keeps a state and provides values for the server protocol module |
| philosopher.erl       | The protocol module that the client provides                                       |
| philosopherServer.erl | A gen server that keeps a state and provides values for the client protocol module |

## Explanation
The example is started in exampleDining.erl, this is an overview of the flow.

1. The server need to be started, this is done with ```waiterServer:start_link() ```
This is a gen_server which will in the init/1 function call ePortListener:start_link(waiter, 19000) in order
to setup a listener that will accept connections.

2. The clients are started with ```philosopherServer:start_link(Id, "localhost") ```
The "Id" is to be able to identify the different philosophers, and "localhost" is just the ip to connect to
and since we're just trying out locally then "localhost" works well.
When a connection is made to the server ePort it will invoke the optional clientConnected function. In this
example the clientConnected will add the connection to a list together with an Id. The Id is naive and assumes
to be the same as the clients (because 1 is started before 2 and so on). This was just to save code space/headache
that isn't relevant to illustrate how ePort works.

3. Sleeps for some time to let the processes work for a while.

4. The waiter sits and waits for the philosophers to ask for chopsticks. The philosophers has a random timeout
that is intended to simulate thinking time and eating time. When a philosopher is done thinking (a timeout
occurs and the philosophers has no chopsticks in its hands) it will ask the waiter for chopsticks. The waiter
will either give back 2 chopsticks or 0 chopsticks based on how many are left and if the philosophers is allowed
to eat or not. Asking for chopsticks is done through the server protocol module: ```waiter:getChopsticks/1```
Chopsticks are given back with ```waiter:takeBackChopsticks/1```

5. Have the waiterServer ask the philosophers how much they have eaten. Asking for the amount of food is
done with: ```philosophers:amountOfFoodEaten/2```
The philosophers will return the amount (time) they have eaten and will then shutdown.
When a philosopher shuts down it will in the terminate/1 function call ePort:close(EPortPid) which will close down
the ePort connection. The ePort connection on the server side (waiterServers EPortPid) will sense this and call
the optional clientDisconnected function in the waiter.erl module.