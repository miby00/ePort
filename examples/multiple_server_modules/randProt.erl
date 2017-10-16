%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(randProt).


%%%===================================================================
%%% Exports
%%%===================================================================
%%% ePort messages
-export([
         clientConnected/3,
         clientDisconnected/2,
         uniform/2
        ]).


%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(protSrvModule, waiterServer).


%%%===================================================================
%%% ePort messages functions
%%%===================================================================
clientConnected(EPortPid, EListenerPid, PeerHost) ->
    io:format("RandProt: ~p is connected~n", [EPortPid]).

clientDisconnected(EPortPid, EListenerPid) ->
    io:format("RandProt: ~p is disconnected~n", [EPortPid]).

uniform(EPortPid, X) ->
    rand:uniform(X).
