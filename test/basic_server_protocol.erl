-module(basic_server_protocol).

-export([ clientConnected/3
        , info/6
        , server_test/1
        ]).

clientConnected(EPid, _ListenerPid, _Host) ->
    basic_server:subscribe(EPid).

info(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    ProtMod = ?MODULE,
    basic_server:info(ProtMod, LogLevel, Module, Function, Args, ErrorDesc, LineNumber).

server_test(_EPid) ->
    {ok, server}.
