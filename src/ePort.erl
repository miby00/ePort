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
         start_init/4,
         start_init/5,
         call/3,
         call/4,
         cast/3,
         cast/4,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([rpcCast/4, rpcCall/5, startWorker/2]).

-define(SERVER,  ?MODULE).
-define(Timeout, 30000).
-define(TCPOptions, [{packet,4}, {active,once}, binary]).

-define(Ping, <<131,100,0,4,112,105,110,103>>). %% term_to_binary(ping, [{minor_version, 1}])
-define(Pong, <<131,100,0,4,112,111,110,103>>). %% term_to_binary(pong),[{minor_version, 1}])

-record(state, {client = false,
                elPid,
                module,
                shutting_down = false,
                socket,
                ssl = false,
                workers = #{},
                timerRef
               }).

-define(log(Level, Args, ErrorDesc), ?log(Level, Args, ErrorDesc, #state{})).
-define(log(Level, Args, ErrorDesc, State),
        log(Level, ?FUNCTION_NAME, Args, ErrorDesc, ?LINE, State)).

-export_type([init_function/0]).

-type init_function() :: fun((pid()) -> term()).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and calls an initializing function.
%% @end
%%--------------------------------------------------------------------
-spec start_init(Module, Host, Port, InitFun)
                -> {ok, Pid} | ignore | {error, Error}
                       when
      Module :: module(),
      Host :: inet:socket_address() | inet:hostname(),
      Port :: inet:port_number(),
      InitFun :: init_function(),
      Pid :: pid(),
      Error :: term().
start_init(Module, Host, Port, InitFun) ->
    start_init(Module, Host, Port, false, InitFun).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server and calls an initializing function.
%% @end
%%--------------------------------------------------------------------
-spec start_init(Module, Host, Port, SSLOptions, InitFun)
                -> {ok, Pid} | ignore | {error, Error}
                       when
      Module :: module(),
      Host :: inet:socket_address() | inet:hostname(),
      Port :: inet:port_number(),
      SSLOptions :: [ssl:tls_client_option()],
      InitFun :: init_function(),
      Pid :: pid(),
      Error :: term().
start_init(Module, Host, Port, SSLOptions, InitFun) ->
    Result =
        case SSLOptions =:= false of
            true -> start(Module, Host, Port);
            false -> start(Module, Host, Port, SSLOptions)
        end,
    case Result of
        {ok, Pid} ->
            try
                _ = InitFun(Pid),
                Result
            catch
                Class:Reason ->
                    stop(Pid),
                    {error, {init_failed, {Class, Reason}}}
            end;
        _ ->
            Result
    end.

call(Pid, Function, Args) ->
    case catch gen_server:call(Pid, {call, Function, Args}) of
        {'EXIT', {timeout, _Reason}} ->
            {error, timeout};
        {'EXIT', _} ->
            {error, connectionClosed};
        Reply ->
            Reply
    end.

call(Pid, Function, Args, Timeout) when is_integer(Timeout) ->
    call(Pid, Function, Args, [{timeout, Timeout}]);
call(Pid, Function, Args, Options) ->
    Timeout   = proplists:get_value(timeout,   Options, 5000),
    ChannelId = proplists:get_value(channelId, Options, undefined),
    Message   = case ChannelId of
                    undefined ->
                        {call, Function, Args};
                    ChannelId ->
                        {call, Function, Args, ChannelId}
                end,
    case catch gen_server:call(Pid, Message, Timeout) of
        {'EXIT', {timeout, _Reason}} ->
            {error, timeout};
        {'EXIT', _} ->
            {error, connectionClosed};
        Reply ->
            Reply
    end.

cast(Pid, Function, Args) ->
    gen_server:cast(Pid, {cast, Function, Args}).

cast(Pid, Function, Args, Options) ->
    ChannelId = proplists:get_value(channelId, Options, undefined),
    case ChannelId of
        undefined ->
            gen_server:cast(Pid, {cast, Function, Args});
        ChannelId ->
            gen_server:cast(Pid, {cast, Function, Args, ChannelId})
    end.

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
            BaseState =
                #state{socket = Socket,
                       client = true,
                       ssl    = false},
            {ok, handleModule(Module, BaseState)};
        Reason ->
            ?log(debug, [Reason, self()],
                 "Shutting down, failed to establish connection."),
            {stop, normal}
    end;
init([Module, Host, Port, {true, SSLOptions}]) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),

    case catch ssl:connect(Host, Port, ?TCPOptions++SSLOptions, 10000) of
        {ok, Socket} ->
            %% Give some time to get protocol negotiation done before
            %% we start sending data.
            timer:sleep(1000),

            erlang:send_after(?Timeout, self(), timeout),
            BaseState =
                #state{socket = Socket,
                       client = true,
                       ssl    = {true, SSLOptions}},
            {ok, handleModule(Module, BaseState)};
        Reason ->
            ?log(debug, [Reason, self()],
                 "Shutting down, failed to establish connection."),
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
handle_call({call, Function, Args}, From, State) ->
    ?log(debug, [Function, self()], "Send call.", State),
    sendPacket({rpc, Function, Args, From}, State);
handle_call({call, Function, Args, Channel}, From, State) ->
    ?log(debug, [Function, Channel, self()], "Send call.", State),
    sendPacket({rpcChan, Function, Args, From, Channel}, State);
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
handle_cast({cast, Function, Args}, State) ->
    ?log(debug, [Function, self()], "Send cast.", State),
    sendPacket({rpc, Function, Args}, State);
handle_cast({cast, Function, Args, Channel}, State) ->
    ?log(debug, [Function, self()], "Send cast.", State),
    sendPacket({rpcChan, Function, Args, Channel}, State);
handle_cast({stop, Offender}, State) ->
    ?log(debug, [State, self(), Offender],
         "Shutting down, stop received.", State),
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
handle_info({rpc_result, Result, From}, State) ->
    sendPacket({rpc_result, Result, From}, State);
handle_info(_Msg, State = #state{shutting_down = true}) ->
    handleShutdown(State);
handle_info({Protocol, Socket, ?Ping}, #state{socket = Socket} = State)
  when Protocol =:= tcp; Protocol =:= ssl ->
    reactivateSocket(State),
    sendPacket(pong, State);
handle_info({Protocol, Socket, ?Pong}, #state{socket = Socket} = State)
  when Protocol =:= tcp; Protocol =:= ssl ->
    #state{timerRef = Ref} = State,
    ?log(debug, [Socket], "Received pong from socket...", State),
    cancelTimer(Ref),
    reactivateSocket(State),
    {noreply, State#state{timerRef = undefined}};
handle_info({tcp, Socket, Data}, State = #state{socket = Socket,
                                                elPid = ELPid,
                                                module = Modules}) when
      is_list(Modules) ->
    case handleDesiredModule(Modules, ELPid, Socket, Data) of
        {bad_module, PModule} ->
            ?log(error, [PModule], "Requested module not allowed", State),
            {stop, normal, State};
        Module ->
            reactivateSocket(State),
            {noreply, State#state{module = Module}}
    end;
handle_info({tcp, Socket, Data}, State = #state{socket = Socket,
                                                module = Module}) ->
    case handlePacket(self(), Data, State) of
        {desiredModule, BadModule} when BadModule /= Module ->
            ?log(error, [BadModule],
                 "Requested module not same as supplied module", State),
            {stop, normal, State};
        {desiredModule, Module} ->
            reactivateSocket(State),
            {noreply, State};
        {ok, State2} ->
            reactivateSocket(State),
            {noreply, State2}
    end;
handle_info({ssl, Socket, Data}, State = #state{socket = Socket,
                                                elPid = ELPid,
                                                module = Modules}) when
      is_list(Modules) ->
    case handleDesiredModule(Modules, ELPid, Socket, Data) of
        {bad_module, PModule} ->
            ?log(error, [PModule], "Requested module not allowed", State),
            {stop, normal, State};
        Module ->
            reactivateSocket(State),
            {noreply, State#state{module = Module}}
    end;
handle_info({ssl, Socket, Data}, State = #state{socket = Socket,
                                                module = Module}) ->
    case handlePacket(self(), Data, State) of
        {desiredModule, BadModule} when BadModule /= Module ->
            ?log(error, [BadModule],
                 "Requested module not same as supplied module",
                 State),
            {stop, normal, State};
        {desiredModule, Module} ->
            reactivateSocket(State),
            {noreply, State};
        {ok, State2} ->
            reactivateSocket(State),
            {noreply, State2}
    end;
handle_info({Closed, Socket}, State = #state{socket = Socket})
  when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    ?log(debug, [State, self()], "Shutting down...received tcp_closed.", State),
    {stop, normal, State};
handle_info(timeout, State = #state{socket = Socket, client = true}) ->
    %% If we havent received a pong signal withing 60 sek, close connection.
    Ref = erlang:send_after(60000, self(), lostConnection),

    erlang:send_after(?Timeout, self(), timeout),

    ?log(debug, [Socket], "Sending ping to socket...", State),

    sendPacket(ping, State#state{timerRef = Ref});
handle_info(timeout, State = #state{client = false}) ->
    {noreply, State};
handle_info(lostConnection, State) ->
    ?log(error, [State, self()],
         "Lost connection signal received, shutting down...", State),
    {stop, normal, State};
handle_info({'DOWN', _MonitorRef,process,_Pid,_}, State) ->
    %% No action needed since monitors are removed automatically upon
    %% process termination.
    {noreply, State};
handle_info({workerShuttingDown, Channel}, State = #state{workers = Workers}) ->
    State2 = State#state{workers = maps:remove(Channel, Workers)},
    {noreply, State2};
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
terminate(Reason, State = #state{socket = undefined}) ->
    ?log(debug, [Reason, self()], "Shutting down...", State),
    ok;
terminate(Reason, State = #state{client = true, socket = Socket}) ->
    ?log(debug, [Reason, Socket, self()], "Shutting down...", State),
    closeSocket(State),
    ok;
terminate(Reason, State = #state{client = false, socket = Socket}) ->
    #state{elPid = ELPid, module = Module} = State,
    %% Tell server protocol implementation that client disconnected.
    (catch Module:clientDisconnected(self(), ELPid)),
    ?log(debug, [Reason, Socket, self()], "Shutting down...", State),
    closeSocket(State),
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
handlePacket(Pid, Data, State = #state{module = Module}) ->
    case binary_to_term(Data) of
        {rpc, Function, Args, From} when is_atom(Function) ->
            spawn_monitor(?MODULE,rpcCall,[Pid,From,Module,Function,Args]),
            {ok, State};
        {rpcChan, Function, Args, From, Channel} when is_atom(Function) ->
            {WorkerPid, State2} = getWorkerPid(Channel, State),
            WorkerPid!{rpc, Pid, Module, Function, Args, From},
            {ok, State2};
        {rpc, Function, Args} when is_atom(Function) ->
            spawn_monitor(?MODULE,rpcCast,[Pid,Module,Function,Args]),
            {ok, State};
        {rpcChan, Function, Args, Channel} when is_atom(Function) ->
            {WorkerPid, State2} = getWorkerPid(Channel, State),
            WorkerPid!{rpc, Pid, Module, Function, Args},
            {ok, State2};
        {rpc_result, Result, From} ->
            gen_server:reply(From, Result),
            {ok, State};
        {desiredModule, ProtocolModule} ->
            {desiredModule, ProtocolModule};
        _ ->
            {ok, State}
    end.

rpcCall(Pid, From, Module, Function, Args) ->
    case catch apply(Module, Function, [Pid|Args]) of
        {'EXIT', Reason} ->
            log(error, ?MODULE, rpcCall, [Reason, self()],
                "rpc call crashed.", ?LINE, Module),
            {error, {noCallbackImplemented, Reason}};
        Result ->
            log(debug, ?MODULE, rpcCall, [self()],
                "rpc call returned...", ?LINE, Module),
            Pid ! {rpc_result, Result, From}
    end.

rpcCast(Pid, Module, Function, Args) ->
    case catch apply(Module, Function, [Pid|Args]) of
        {'EXIT', Reason} ->
            log(error, ?MODULE, rpcCast, [Reason, self()],
                "rpc cast crashed.", ?LINE, Module),
            {error, {noCallbackImplemented, Reason}};
        Result ->
            log(debug, ?MODULE, rpcCast, [self()],
                "rpc cast returned...", ?LINE, Module),
            Result
    end.

reactivateSocket(#state{socket = Socket, ssl = false}) ->
    inet:setopts(Socket, [{active, once}]);
reactivateSocket(#state{socket = Socket}) ->
    ssl:setopts(Socket, [{active, once}]).

sendPacket(Term, #state{socket = Socket, ssl = SSL} = State) ->
    SendFun =
        case SSL =:= false of
            true -> fun gen_tcp:send/2;
            false -> fun ssl:send/2
        end,
    case ok =:= SendFun(Socket, term_to_binary(Term, [{minor_version, 1}])) of
        true -> {noreply, State};
        false ->
            ?log(error, [self(), State], "send packet failed", State),
            {stop, normal, State}
    end.

closeSocket(#state{socket = Socket, ssl = false}) ->
    catch gen_tcp:close(Socket);
closeSocket(#state{socket = Socket}) ->
    catch ssl:close(Socket).

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


handleModule({LocalModule, RemoteModule}, State) ->
    sendPacket({desiredModule, RemoteModule}, State),
    State#state{module = LocalModule};
handleModule(Module, State) when is_atom(Module) ->
    State#state{module = Module}.

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
    case catch binary_to_term(Data) of
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

getWorkerPid(Channel, State = #state{workers = Workers}) ->
    case maps:get(Channel, Workers, undefined) of
        undefined ->
            Pid = spawn(?MODULE, startWorker, [self(), Channel]),
            {Pid, State#state{workers = Workers#{Channel => Pid}}};
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    {Pid, State};
                false ->
                    Pid2 = spawn(?MODULE, startWorker, [self(), Channel]),
                    {Pid2, State#state{workers = Workers#{Channel => Pid2}}}
            end
    end.

startWorker(Pid, Channel) ->
    monitor(process, Pid),
    worker(Pid, Channel).

worker(Parent, Channel) ->
    receive
        {rpc, Pid, Module, Function, Args, From} ->
            rpcCall(Pid, From, Module, Function, Args),
            worker(Parent, Channel);
        {rpc, Pid, Module, Function, Args} ->
            rpcCast(Pid, Module, Function, Args),
            worker(Parent, Channel);
        {'DOWN', _MonitorRef,process,_Pid,_} ->
            log(debug, ?MODULE, worker, [Parent, Channel],
                "Shutting down, received down from parent.",
                ?LINE, undefined),
            Parent!{workerShuttingDown, Channel},
            ok;
        stop ->
            Parent!{workerShuttingDown, Channel},
            ok;
        _Unknown ->
            worker(Parent, Channel)
    after 2 * 3600 * 1000 ->
            log(debug, ?MODULE, worker, [Parent, Channel],
                "Shutting down, inactive for 2 hours.",
                ?LINE, undefined),
            Parent!{workerShuttingDown, Channel},
            ok
    end.

log(LogLevel, Function, Args, ErrorDesc, LineNumber, State) ->
    #state{module = Module} = State,
    log(LogLevel, ?MODULE, Function, Args, ErrorDesc, LineNumber, Module).

log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber, ProtModule) ->
    eLog:log(LogLevel, Module, Function, Args, ErrorDesc, LineNumber, ProtModule).
