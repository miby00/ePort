%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(exampleMultiple).


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
    Port = 19001,
    {ok, _SrvPid} =
        ePortListener:start_link([listProt, randProt], Port),
    {ok, ListPid} =
        ePort:start_link({undefined, listProt}, "localhost", Port),
    {ok, RandPid} =
        ePort:start_link({undefined, randProt}, "localhost", Port),

    io:format("ListPid: ~p~n", [ePort:call(ListPid, append, [[1,2], [3,4]])]),
    io:format("RandPid: ~p~n", [ePort:call(RandPid, uniform, [1000])]),
    exit(ListPid, kill),
    exit(RandPid, kill).
