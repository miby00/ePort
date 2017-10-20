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
    {ok,SrvPid}   = ePortListener:start_link(listProt, Port),
    {ok,ListPid}  = ePort:start_link({undefined,listProt},"localhost",Port),
    {ok,RandPid1} = ePort:start_link({undefined,randProt},"localhost",Port),
    ePortListener:updateModulesAllowed(SrvPid, [listProt, randProt]),
    {ok,RandPid2} = ePort:start_link({undefined,randProt},"localhost",Port),

    %% Expected value: [1,2,3,4]
    io:format("ListPid: ~p~n", [ePort:call(ListPid,append,[[1,2],[3,4]])]),

    %% Expected value: {error, connectionClosed}
    io:format("RandPid1: ~p~n", [ePort:call(RandPid1, uniform, [1000])]),

    %% Expected value: random number between 1 and 1000
    io:format("RandPid2: ~p~n", [ePort:call(RandPid2, uniform, [1000])]),
    ePort:stop(ListPid),
    ePort:stop(RandPid1),
    ePort:stop(RandPid2),
    ePortListener:stop(SrvPid),
    ok.
