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
         start/4
        ]).


%%%===================================================================
%%% API
%%%===================================================================
start(Module, ListenSocket, AllowedIps, SSLConfig) ->
    LPid = self(),
    Fun = fun() ->
                  doAccept(LPid,
                           Module,
                           ListenSocket,
                           AllowedIps,
                           SSLConfig)
          end,
    spawn(Fun).


%%%===================================================================
%%% Internal functions
%%%===================================================================
doAccept(LPid, Module, ListenSocket, AllowedIps, false) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            startPort(LPid, Module, Socket, AllowedIps, false);
        Reason ->
            eLog:log(debug, ?MODULE, doAccept, [Reason],
                     "Received error from accept", ?LINE, Module),
            Reason
    end,
    LPid ! nextWorker;
doAccept(LPid, Module, ListenSocket, AllowedIps, {true, SSLOptions}) ->
    case ssl:transport_accept(ListenSocket) of
        {ok, Socket} ->
            case catch ssl:ssl_accept(Socket) of
                ok ->
                    startPort(LPid, Module, Socket, AllowedIps,
                              {true, SSLOptions});
                _RetValue ->
                    ok
            end;
        Reason ->
            eLog:log(debug, ?MODULE, doAccept, [Reason],
                     "Received error from accept", ?LINE, Module),
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
