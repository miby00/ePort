%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(waiter).


%%%===================================================================
%%% Exports
%%%===================================================================
%%% ePort messages
-export([
         clientConnected/3,
         clientDisconnected/2,
         getChopsticks/1,
         takeBackChopsticks/1
        ]).


%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(protSrvModule, waiterServer).


%%%===================================================================
%%% ePort messages functions
%%%===================================================================
clientConnected(EPortPid, EListenerPid, PeerHost) ->
    ?protSrvModule:clientConnected(EPortPid, EListenerPid, PeerHost).

clientDisconnected(EPortPid, EListenerPid) ->
    ?protSrvModule:clientDisconnected(EPortPid, EListenerPid).

getChopsticks(EPortPid) ->
    ?protSrvModule:getChopsticks(EPortPid).

takeBackChopsticks(EPortPid) ->
    ?protSrvModule:takeBackChopsticks(EPortPid).
