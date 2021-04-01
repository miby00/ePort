-module(ePort_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
all() ->
    [ basic
    , disconnect_client
    , disconnect_server
    ].

-define(PORT, 12345).

%%--------------------------------------------------------------------
basic(Config) when is_list(Config) ->
    {Server, Listener, ServerPort, ClientPort} = basic_pre(),
    ?assertEqual({ok, server}, ePort:call(ServerPort, server_test, [])),
    ?assertEqual({ok, client}, ePort:call(ClientPort, client_test, [])),
    ePort:stop(ClientPort),
    ePort:stop(ServerPort),
    basic_post(Server, Listener).

%%--------------------------------------------------------------------
disconnect_client(Config) when is_list(Config) ->
    {Server, Listener, ServerPort, ClientPort} = basic_pre(),
    ePort:stop(ServerPort),
    ?assertEqual({error, connectionClosed}, ePort:call(ClientPort, client_test, [])),
    ePort:stop(ClientPort),
    basic_post(Server, Listener).

%%--------------------------------------------------------------------
disconnect_server(Config) when is_list(Config) ->
    {Server, Listener, ServerPort, ClientPort} = basic_pre(),
    ePort:stop(ClientPort),
    ?assertEqual({error, connectionClosed}, ePort:call(ServerPort, server_test, [])),
    ePort:stop(ServerPort),
    basic_post(Server, Listener).

%%--------------------------------------------------------------------
basic_pre() ->
    process_flag(trap_exit, true),
    ServerMod = basic_server_protocol,
    ClientMod = basic_client_protocol,
    Host = "localhost",
    {ok, Server} = basic_server:start_link(),
    {ok, Listener} = ePortListener:start_link(ServerMod, ?PORT),
    sys:trace(Listener, true),
    {ok, ServerPort} = ePort:start_link(ClientMod, Host, ?PORT),
    sys:trace(ServerPort, true),
    ClientPort = basic_server:getPort(Server),
    sys:trace(ClientPort, true),
    link(ClientPort),
    Pids =
        [ {test, self()}
        , {server, Server}
        , {listener, Listener}
        , {sport, ServerPort}
        , {cport, ClientPort}
        ],
    ct:log("~p~n", [Pids]),
    {Server, Listener, ServerPort, ClientPort}.

basic_post(Server, Listener) ->
    ePortListener:stop(Listener),
    gen_server:stop(Server),
    no_bad_dead().

no_bad_dead() ->
    receive
        {'EXIT', _, normal} -> no_bad_dead();
        Any -> error({got_abnormal_signal, Any})
    after
        0 -> ok
    end.
