-module(basic_client_protocol).

-export([ client_test/1
        , info/6
        ]).

client_test(_EPid) ->
    {ok, client}.

info(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    ProtMod = ?MODULE,
    basic_server:info(ProtMod, LogLevel, Module, Function, Args, ErrorDesc, LineNumber).
