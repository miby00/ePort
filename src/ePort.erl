%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(ePort).

-behaviour(gen_server).

%% API
-export([start_link/3,
         start_link/4,
         start/3,
         start/4,
         call/3,
         call/4,
         cast/3,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([rpcCast/4, rpcCall/5]).

-define(SERVER,  ?MODULE).
-define(Timeout, 30000).
-define(TCPOptions, [{packet,4}, {active,once}, binary]).

-define(Ping,    <<131,100,0,4,112,105,110,103>>).
-define(Pong,    <<131,100,0,4,112,111,110,103>>).

-record(state, {client = false,
                elPid,
                module,
                shutting_down = false,
                socket,
                ssl = false,
                timerRef
               }).

-import(eLog, [log/7]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server.
%% Module can be either an atom or a tuple with two elements with the
%% local protocol module and the desired remote protocol module.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ELPid, Module, Socket) when is_pid(ELPid) ->
    gen_server:start_link(?MODULE, [ELPid, Module, Socket, false], []);
start_link(Module, Host, Port) ->
    gen_server:start_link(?MODULE, [Module, Host, Port, false], []).

start_link(ELPid, Module, Socket, SSLOptions) when is_pid(ELPid) ->
    gen_server:start_link(?MODULE, [ELPid, Module, Socket,
                                    {true, SSLOptions}], []);
start_link(Module, Host, Port, SSLOptions) ->
    gen_server:start_link(?MODULE,[Module,Host,Port,{true,SSLOptions}], []).

start(Module, Host, Port) ->
    gen_server:start(?MODULE, [Module, Host, Port, false], []).

start(Module, Host, Port, SSLOptions) ->
    gen_server:start(?MODULE, [Module, Host, Port, {true, SSLOptions}], []).

call(Pid, Function, Args) ->
    case catch gen_server:call(Pid, {call, Function, Args}) of
        {'EXIT', {timeout, _Reason}} ->
            {error, timeout};
        {'EXIT', _} ->
            {error, connectionClosed};
        Reply ->
            Reply
    end.

call(Pid, Function, Args, Timeout) ->
    case catch gen_server:call(Pid, {call, Function, Args}, Timeout) of
        {'EXIT', {timeout, _Reason}} ->
            {error, timeout};
        {'EXIT', _} ->
            {error, connectionClosed};
        Reply ->
            Reply
    end.

cast(Pid, Function, Args) ->
    gen_server:cast(Pid, {cast, Function, Args}).

stop(Pid) ->
    gen_server:cast(Pid, {stop, self()}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ELPid, Module, Socket, false]) when is_pid(ELPid) ->
    inet:setopts(Socket, [{packet, 4}, {active,once}, binary]),

    IPAddr = ePortListener:getIpAddress(Socket),
    (catch Module:clientConnected(self(), ELPid, IPAddr)),
    {ok, #state{socket = Socket,
                module = Module,
                elPid  = ELPid,
                ssl    = false}};
init([ELPid, Module, Socket, SSLConfig]) when is_pid(ELPid) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    ssl:setopts(Socket, [{packet, 4}, {active,once}, binary]),

    IPAddr = ePortListener:getIpAddress(Socket),
    (catch Module:clientConnected(self(), ELPid, IPAddr)),
    {ok, #state{socket = Socket,
                module = Module,
                elPid  = ELPid,
                ssl    = SSLConfig}};
init([Module, Host, Port, false]) ->
    case catch gen_tcp:connect(Host, Port, ?TCPOptions, 10000) of
        {ok, Socket} ->
            erlang:send_after(?Timeout, self(), timeout),
            {ok, #state{socket = Socket,
                        module = handleModule(Socket, Module, tcp),
                        client = true,
                        ssl    = false}};
        Reason ->
            log(debug, ?MODULE, init, [Reason, self()],
                "Shutting down, failed to establish connection.",
                ?LINE, Module),
            {stop, normal}
    end;
init([Module, Host, Port, {true, SSLOptions}]) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),

    case catch ssl:connect(Host, Port, ?TCPOptions++SSLOptions, 10000) of
        {ok, Socket} ->
            erlang:send_after(?Timeout, self(), timeout),
            {ok, #state{socket = Socket,
                        module = handleModule(Socket, Module, ssl),
                        client = true,
                        ssl    = {true, SSLOptions}}};
        Reason ->
            log(debug, ?MODULE, init, [Reason, self()],
                "Shutting down, failed to establish connection.",
                ?LINE, Module),
            {stop, normal}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State = #state{shutting_down = true}) ->
    handleShutdown(State);
handle_call({call, Function, Args}, From, State = #state{socket = Socket,
                                                         module = Module,
                                                         ssl    = false}) ->
    log(debug, ?MODULE, handle_call, [Function, self()],
        "Send call.", ?LINE, Module),
    doSendPacket(Socket, {rpc, Function, Args, From}),
    {noreply, State};
handle_call({call, Function, Args}, From, State = #state{socket = Socket,
                                                         module = Module}) ->
    log(debug, ?MODULE, handle_call, [Function, self()],
        "Send call.", ?LINE, Module),
    doSendPacketSSL(Socket, {rpc, Function, Args, From}),
    {noreply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State = #state{shutting_down = true}) ->
    handleShutdown(State);
handle_cast({cast, Function, Args}, State = #state{socket = Socket,
                                                   module = Module,
                                                   ssl    = false}) ->
    log(debug, ?MODULE, handle_cast, [Function, self()],
        "Send cast.", ?LINE, Module),
    doSendPacket(Socket, {rpc, Function, Args}),
    {noreply, State};
handle_cast({cast, Function, Args}, State = #state{socket = Socket,
                                                   module = Module}) ->
    log(debug, ?MODULE, handle_cast, [Function, self()],
        "Send cast.", ?LINE, Module),
    doSendPacketSSL(Socket, {rpc, Function, Args}),
    {noreply, State};
handle_cast({stop, Offender}, State = #state{module = Module}) ->
    log(debug, ?MODULE, stop, [State, self(), Offender],
        "Shutting down, stop received.", ?LINE, Module),
    handleShutdown(State#state{shutting_down = true});
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({rpc_result, Result, From}, State = #state{socket = Socket,
                                                       ssl    = false}) ->
    doSendPacket(Socket, {rpc_result, Result, From}),
    {noreply, State};
handle_info({rpc_result, Result, From}, State = #state{socket = Socket}) ->
    doSendPacketSSL(Socket, {rpc_result, Result, From}),
    {noreply, State};
handle_info(_Msg, State = #state{shutting_down = true}) ->
    handleShutdown(State);
handle_info({tcp, Socket, ?Ping}, State = #state{socket = Socket,
                                                 ssl    = false}) ->
    doSendPacket(Socket, pong),
    inet:setopts(Socket,[{active,once}]),
    {noreply, State};
handle_info({ssl, Socket, ?Ping}, State = #state{socket = Socket}) ->
    doSendPacketSSL(Socket, pong),
    ssl:setopts(Socket,[{active,once}]),
    {noreply, State};
handle_info({tcp, Socket, ?Pong}, State = #state{socket   = Socket,
                                                 timerRef = Ref,
                                                 ssl      = false,
                                                 module   = Module}) ->

    log(debug, ?MODULE, handle_info, [Socket],
        "Received pong from socket...", ?LINE, Module),

    cancelTimer(Ref),

    inet:setopts(Socket,[{active,once}]),
    {noreply, State#state{timerRef = undefined}};
handle_info({ssl, Socket, ?Pong}, State = #state{socket   = Socket,
                                                 timerRef = Ref,
                                                 module   = Module}) ->

    log(debug, ?MODULE, handle_info, [Socket],
        "Received pong from socket...", ?LINE, Module),

    cancelTimer(Ref),

    ssl:setopts(Socket,[{active,once}]),
    {noreply, State#state{timerRef = undefined}};
handle_info({tcp, Socket, Data}, State = #state{socket = Socket,
                                                elPid = ELPid,
                                                module = Modules}) when
                   is_list(Modules) ->
    case handleDesiredModule(Modules, ELPid, Socket, Data) of
        {bad_module, PModule} ->
            eLog:log(error, ?MODULE, handle_info, [PModule],
                "Requested module not allowed", ?LINE),
            {stop, normal, State};
        Module ->
            inet:setopts(Socket,[{active,once}]),
            {noreply, State#state{module = Module}}
    end;
handle_info({tcp, Socket, Data}, State = #state{socket = Socket,
                                                module = Module}) ->
    case handlePacket(self(), Module, Data) of
        {desiredModule, BadModule} when BadModule /= Module ->
            eLog:log(error, ?MODULE, handle_info, [BadModule],
                     "Requested module not same as supplied module",
                     ?LINE),
            {stop, normal, State};
        _Else ->
            inet:setopts(Socket,[{active,once}]),
            {noreply, State}
    end;
handle_info({ssl, Socket, Data}, State = #state{socket = Socket,
                                                elPid = ELPid,
                                                module = Modules}) when
                   is_list(Modules) ->
    case handleDesiredModule(Modules, ELPid, Socket, Data) of
        {bad_module, PModule} ->
            eLog:log(error, ?MODULE, handle_info, [PModule],
                "Requested module not allowed", ?LINE),
            {stop, normal, State};
        Module ->
            ssl:setopts(Socket,[{active,once}]),
            {noreply, State#state{module = Module}}
    end;
handle_info({ssl, Socket, Data}, State = #state{socket = Socket,
                                                module = Module}) ->
    case handlePacket(self(), Module, Data) of
        {desiredModule, BadModule} when BadModule /= Module ->
            eLog:log(error, ?MODULE, handle_info, [BadModule],
                     "Requested module not same as supplied module",
                     ?LINE),
            {stop, normal, State};
        _Else ->
            ssl:setopts(Socket,[{active,once}]),
            {noreply, State}
    end;
handle_info({tcp_closed, Socket}, State = #state{socket = Socket,
                                                 module = Module}) ->
    log(debug, ?MODULE, handle_info, [State, self()],
        "Shutting down...received tcp_closed.", ?LINE, Module),
    {stop, normal, State};
handle_info({ssl_closed, Socket}, State = #state{socket = Socket,
                                                 module = Module}) ->
    log(debug, ?MODULE, handle_info, [State, self()],
        "Shutting down...received tcp_closed.", ?LINE, Module),
    {stop, normal, State};
handle_info(timeout, State = #state{socket = Socket,
                                    client = true,
                                    ssl    = false,
                                    module = Module}) ->
    doSendPacket(Socket, ping),

    %% If we havent received a pong signal withing 60 sek, close connection.
    Ref = erlang:send_after(60000, self(), lostConnection),

    erlang:send_after(?Timeout, self(), timeout),

    log(debug, ?MODULE, handle_info, [Socket],
        "Sending ping to socket...", ?LINE, Module),

    {noreply, State#state{timerRef = Ref}};
handle_info(timeout, State = #state{socket = Socket,
                                    client = true,
                                    module = Module}) ->
    doSendPacketSSL(Socket, ping),

    %% If we havent received a pong signal withing 60 sek, close connection.
    Ref = erlang:send_after(60000, self(), lostConnection),

    erlang:send_after(?Timeout, self(), timeout),

    log(debug, ?MODULE, handle_info, [Socket],
        "Sending ping to socket...", ?LINE, Module),

    {noreply, State#state{timerRef = Ref}};
handle_info(timeout, State = #state{client = false}) ->
    {noreply, State};
handle_info(lostConnection, State = #state{module = Module}) ->
    log(error, ?MODULE, handle_info, [State, self()],
        "Lost connection signal received, shutting down...", ?LINE, Module),
    {stop, normal, State};
handle_info({'DOWN', _MonitorRef,process,_Pid,_}, State) ->
    %% No action needed since monitors are removed automatically upon
    %% process termination.
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{socket = undefined,
                         module = Module}) ->
    log(debug, ?MODULE, terminate, [Reason, self()],
        "Shutting down...", ?LINE, Module),
    ok;
terminate(Reason, #state{client = true,
                         module = Module,
                         socket = Socket,
                         ssl    = SSL}) ->
    log(debug, ?MODULE, terminate, [Reason, Socket, self()],
        "Shutting down...", ?LINE, Module),
    case SSL of
        false ->
            (catch gen_tcp:close(Socket));
        _ ->
            (catch ssl:close(Socket))
    end,
    ok;
terminate(Reason, #state{client = false,
                         elPid  = ELPid,
                         module = Module,
                         socket = Socket,
                         ssl    = SSL}) ->
    %% Tell server protocol implementation that client disconnected.
    (catch Module:clientDisconnected(self(), ELPid)),
    log(debug, ?MODULE, terminate, [Reason, Socket, self()],
        "Shutting down...", ?LINE, Module),
    case SSL of
        false ->
            (catch gen_tcp:close(Socket));
        _ ->
            (catch ssl:close(Socket))
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handlePacket(Pid, Module, Data) ->
    case binary_to_term(Data) of
        {rpc, Function, Args, From} when is_atom(Function) ->
            spawn_monitor(?MODULE,rpcCall,[Pid,From,Module,Function,Args]);
        {rpc, Function, Args} when is_atom(Function) ->
            spawn_monitor(?MODULE,rpcCast,[Pid,Module,Function,Args]);
        {rpc_result, Result, From} ->
            gen_server:reply(From, Result);
        {desiredModule, ProtocolModule} ->
            {desiredModule, ProtocolModule};
        _ ->
            ok
    end.

rpcCall(Pid, From, Module, Function, Args) ->
    case catch apply(Module, Function, [Pid|Args]) of
        {'EXIT', Reason} ->
            log(error, ?MODULE, rpcCall, [Reason, self()],
                "rpc call crashed.", ?LINE, Module),
            {error, {noCallbackImplemented, Reason}};
        Result ->
            log(debug, ?MODULE, rpcCall, [Result, self()],
                "rpc call returns...", ?LINE, Module),
            Pid ! {rpc_result, Result, From}
    end.

rpcCast(Pid, Module, Function, Args) ->
    case catch apply(Module, Function, [Pid|Args]) of
        {'EXIT', Reason} ->
            log(error, ?MODULE, rpcCast, [Reason, self()],
                "rpc cast crashed.", ?LINE, Module),
            {error, {noCallbackImplemented, Reason}};
        Result ->
            log(debug, ?MODULE, rpcCast, [Result, self()],
                "rpc cast returns...", ?LINE, Module),
            Result
    end.

doSendPacket(Socket, Term) ->
    ok = gen_tcp:send(Socket, term_to_binary(Term)).

doSendPacketSSL(Socket, Term) ->
    ok = ssl:send(Socket, term_to_binary(Term)).

cancelTimer(undefined) ->
    ok;
cancelTimer(Ref) ->
    %% Recieved pong, cancel timer.
    erlang:cancel_timer(Ref).

handleShutdown(State) ->
    case erlang:process_info(self(), [monitors]) of
        [{monitors, []}] -> {stop, normal, State};
        _                -> {noreply, State}
    end.


handleModule(Socket, {LocalModule, RemoteModule}, tcp) ->
    doSendPacket(Socket, {desiredModule, RemoteModule}),
    LocalModule;
handleModule(Socket, {LocalModule, RemoteModule}, ssl) ->
    doSendPacketSSL(Socket, {desiredModule, RemoteModule}),
    LocalModule;
handleModule(_Socket, Module_ Type) when is_atom(Module) ->
    Module.


handleDesiredModule(Modules, ELPid, Socket, {desiredModule, PModule}) ->
    case lists:member(PModule, Modules) of
        true  ->
            IPAddr = ePortListener:getIpAddress(Socket),
            (catch PModule:clientConnected(self(), ELPid, IPAddr)),
            PModule;
        false ->
            log(debug, ?MODULE, handleDesiredModule, [PModule, self()],
                "Desired module is not allowed or not specified",
                ?LINE, undefined),
            {bad_module, PModule}
    end;
handleDesiredModule(Modules, ELPid, Socket, Data) ->
    case handlePacket(self(), Modules, Data) of
        {desiredModule, PModule} when is_atom(PModule) ->
            %% The parsed data is sent into this same function
            %% for further processing.
            handleDesiredModule(Modules,
                               ELPid,
                               Socket,
                               {desiredModule, PModule});
        _ ->
            %% This scenario should not happen, but if for some strange
            %% reason it happens anyway we return bad_module and then
            %% the connection will be closed.
            {bad_module, undefined}
    end.
