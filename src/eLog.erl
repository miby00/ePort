%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 14 Jul 2012 by Mikael Bylund
%%%-------------------------------------------------------------------
-module(eLog).

-define(Type, info).

-export([log/6, log/7]).

log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber, undefined).

log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber, ProtModule) ->
    case ?Type of
        io ->
            io:format("~nLog time: ~p~nModule: ~p; Function: ~p; "
                      "Linenumber: ~p~n" ++ ErrorDesc ++ "~nArgs: ~p~n",
                      [time(), Module, Function, LineNumber , [Args]]);
        file ->
            (catch eLogWriter:log(LogLevel, Module, Function,
                                  Args, ErrorDesc, LineNumber));
        info ->
            info(ProtModule, {LogLevel, Module, Function, Args,
                              ErrorDesc, LineNumber});
        none ->
            ok
    end.

info(ProtModule, InfoArgs) when is_atom(ProtModule) ->
    info2(ProtModule, InfoArgs);
info({LocalProtModule, _RemoteProtModule}, InfoArgs)
  when is_atom(LocalProtModule) ->
    info2(LocalProtModule, InfoArgs);
info(ProtModules, InfoArgs) when is_list(ProtModules) ->
    [info2(ProtModule, InfoArgs) || ProtModule <- ProtModules,
                                    is_atom(ProtModule)],
    ok;
info(_ProtModule, _InfoArgs) ->
    error.

info2(undefined, _InfoArgs) ->
    ok;
info2(ProtModule, {LogLevel, Module, Function, Args, ErrorDesc, LineNumber}) ->
    (catch ProtModule:info(LogLevel, Module, Function,
                           Args, ErrorDesc, LineNumber)).
