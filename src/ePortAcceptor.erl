%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, mikael
%%% @doc
%%%
%%% @end
%%% Created : 20 Jun 2017 by Taddic <tommy.mattsson@gmail.com>
%%%-------------------------------------------------------------------
-module(ePortAcceptor).

%% API
-export([
         start/2
        ]).


%%%===================================================================
%%% API
%%%===================================================================
start(ListenSocket, SSLConfig) ->
    LPid = self(),
    Fun = fun() -> doAccept(LPid, ListenSocket, SSLConfig) end,
    spawn(Fun).


%%%===================================================================
%%% Internal functions
%%%===================================================================
doAccept(LPid, ListenSocket, false) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            ListenConfig = ePortListener:getConfig(LPid),
            AllowedIps = proplists:get_value(allowedIps, ListenConfig),
            Module = proplists:get_value(protocolModules, ListenConfig),
            startPort(LPid, Module, Socket, AllowedIps, false);
        Reason ->
            eLog:log(debug, ?MODULE, doAccept, [Reason],
                     "Received error from accept", ?LINE),
            Reason
    end,
    LPid ! nextWorker;
doAccept(LPid, ListenSocket, {true, SSLOptions}) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, TransportSocket} ->
            case catch ssl:handshake(TransportSocket) of
                {ok, Socket} ->
                    ListenConfig = ePortListener:getConfig(LPid),
                    AllowedIps = proplists:get_value(allowedIps,ListenConfig),
                    Module = proplists:get_value(protocolModules,ListenConfig),
                    startPort(LPid, Module, Socket, AllowedIps,
                              {true, SSLOptions});
                RetValue ->
                    eLog:log(error, ?MODULE, doAccept, [RetValue],
                             "Handshake error", ?LINE),
                    ok
            end;
        Reason ->
            eLog:log(error, ?MODULE, doAccept, [Reason],
                     "Received error from accept", ?LINE),
            Reason
    end,
    LPid ! nextWorker.

startPort(LPid, Module, Socket, AllowedIps, false) ->
    case allowedIp(Socket, AllowedIps) of
        {true, IP} ->
            eLog:log(debug, ?MODULE, startPort, [LPid, IP],
                     "Connect attempt", ?LINE, Module),
            case ePort:start(LPid, Module, Socket) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid);
                Reason ->
                    Reason
            end;
        {false, IP} ->
            eLog:log(debug, ?MODULE, startPort, [LPid, IP],
                     "Illegal connect attempt", ?LINE, Module),
            gen_tcp:close(Socket),
            {error, notAllowedIp}
    end;
startPort(LPid, Module, Socket, AllowedIps, {true, SSLOptions}) ->
    case allowedIp(Socket, AllowedIps) of
        {true, IP} ->
            eLog:log(debug, ?MODULE, startPort, [LPid, IP],
                     "Connect attempt", ?LINE, Module),
            case ePort:start(LPid, Module, Socket, SSLOptions) of
                {ok, Pid} ->
                    ssl:controlling_process(Socket, Pid);
                Reason ->
                    Reason
            end;
        {false, IP} ->
            eLog:log(debug, ?MODULE, startPort, [LPid, IP],
                     "Illegal connect attempt", ?LINE, Module),
            ssl:close(Socket),
            {error, notAllowedIp}
    end.

allowedIp(Socket,  AllowedIps) when is_list(AllowedIps)->
    IP = ePortListener:getIpAddress(Socket),
    {lists:member(IP, AllowedIps), IP};
allowedIp(Socket,  _) ->
    IP = ePortListener:getIpAddress(Socket),
    {true, IP}.
