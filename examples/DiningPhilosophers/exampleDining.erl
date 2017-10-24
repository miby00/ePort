%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(exampleDining).


%%%===================================================================
%%% Exports
%%%===================================================================
%%% API
-export([
         start/0
        ]).


%%%===================================================================
%%% API functions
%%%===================================================================
start() ->
    %% Start the listener on the server side
    {ok, WaiterServer} = waiterServer:start_link(),

    %% Start five philosophers that want to eat.
    lists:foreach(fun(Id) ->
                          philosopherServer:start_link(Id, "localhost")
                  end, [1,2,3,4,5]),

    %% Let the philosophers think and eat for some time
    timer:sleep(30000),

    %% Request the amount of food eaten from each philosophers,
    %% each philosopher will terminate after this.
    lists:foreach(fun waiterServer:amountOfFoodEaten/1, [1,2,3,4,5]),

    %% Shut down the waiter server
    gen_server:stop(WaiterServer).
