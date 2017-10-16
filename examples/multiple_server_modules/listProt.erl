%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(listProt).


%%%===================================================================
%%% Exports
%%%===================================================================
%%% ePort messages
-export([
         clientConnected/3,
         clientDisconnected/2,
         append/3
        ]).


%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(protSrvModule, waiterServer).


%%%===================================================================
%%% ePort messages functions
%%%===================================================================
clientConnected(EPortPid, _EListenerPid, _PeerHost) ->
    io:format("ListProt: ~p is connected~n", [EPortPid]).

clientDisconnected(EPortPid, _EListenerPid) ->
    io:format("ListProt: ~p is disconnected~n", [EPortPid]).

append(EPortPid, List1, List2) ->
    lists:append(List1, List2).
