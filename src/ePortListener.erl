%%%-------------------------------------------------------------------
%%% @author Mikael Bylund <mikael.bylund@gmail.com>
%%% @copyright (C) 2012, mikael
%%% @doc
%%%
%%% @end
%%% Created : 16 Jun 2012 by Mikael Bylund <mikael.bylund@gmail.com>
%%%-------------------------------------------------------------------
-module(ePortListener).

-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3,
         start_link/4,
         updateIPAllowed/2,
         updateModulesAllowed/2,
         updateListenPort/2,
         disable/1,
         enable/1,
         stop/1]).

%% Utility functions
-export([getIpAddress/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(TCPOptions, [{active, false}, {packet, 4}]).
-define(Timeout, 30000).

-record(state, {allowedIps = all,
                disabled = false,
                listenSocket,
                protocolModule,
                ssl = false,
                worker = undefined
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module, LPort) ->
    gen_server:start_link(?MODULE, [Module, LPort, all], []).

start_link(Module, LPort, AllowedIps) ->
    gen_server:start_link(?MODULE, [Module, LPort, AllowedIps], []).

start_link(Module, LPort, AllowedIps, SSLOptions) ->
    gen_server:start_link(?MODULE,[Module,LPort,AllowedIps,SSLOptions],[]).

updateIPAllowed(Pid, IPAllowed) ->
    gen_server:call(Pid, {updateIPAllowed, IPAllowed}, ?Timeout).

updateModulesAllowed(Pid, Modules) ->
    gen_server:call(Pid, {updateModulesAllowed, Modules}, ?Timeout).

updateListenPort(Pid, Port) ->
    gen_server:call(Pid, {updateListenPort, Port}, ?Timeout).

disable(Pid) ->
    gen_server:cast(Pid, disable).

enable(Pid) ->
    gen_server:cast(Pid, enable).

stop(Pid) ->
    gen_server:call(Pid, stop, ?Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server. The server can be initialized either with
%% a single protocol module (atom) or several allowed protocol modules
%% (a list of atoms).
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module, LPort, AllowedIps]) ->
    eLog:log(debug, ?MODULE, init, [Module, LPort, AllowedIps],
             "Starting server...", ?LINE, Module),
    case gen_tcp:listen(LPort, ?TCPOptions) of
        {ok, ListenSocket} ->
            self() ! nextWorker,
            {ok, #state{ssl            = false,
                        listenSocket   = ListenSocket,
                        protocolModule = Module,
                        allowedIps     = AllowedIps}};
        {error, Reason} ->
            eLog:log(debug, ?MODULE, init, [LPort, Reason],
                     "Cannot start listen port...", ?LINE, Module),
            {stop, Reason}
    end;
init([Module, LPort, AllowedIps, SSLOptions]) ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),

    eLog:log(debug, ?MODULE, init, [Module, LPort, AllowedIps],
             "Starting server...", ?LINE, Module),

    case ssl:listen(LPort, ?TCPOptions ++ SSLOptions) of
        {ok, ListenSocket} ->
            self() ! nextWorker,
            {ok, #state{ssl            = {true, SSLOptions},
                        listenSocket   = ListenSocket,
                        protocolModule = Module,
                        allowedIps     = AllowedIps}};
        {error, Reason} ->
            eLog:log(debug, ?MODULE, init, [LPort, Reason],
                     "Cannot start listen port...", ?LINE, Module),
            {stop, Reason}
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
handle_call({updateIPAllowed, IPAllowed}, _From, State) when
      is_list(IPAllowed) ->
    eLog:log(debug, ?MODULE, handle_call, [IPAllowed],
             "Updating allowed IPs", ?LINE),
    updateListener(State#state{allowedIps = IPAllowed});

handle_call({updateModulesAllowed, Modules}, _From, State) when
      is_list(Modules) ->
    eLog:log(debug, ?MODULE, handle_call, [Modules],
             "Updating allowed protocol modules", ?LINE),
    updateListener(State#state{protocolModule = Modules});

handle_call({updateListenPort, Port}, _From,
            State = #state{ssl = false}) when is_integer(Port) ->
    eLog:log(debug, ?MODULE, handle_call, [Port],
             "Updating listen port", ?LINE),
    updateListenPort(gen_tcp, [], Port, State);
handle_call({updateListenPort, Port}, _From,
            State = #state{ssl = {true,SSLOptions}}) when is_integer(Port) ->
    eLog:log(debug, ?MODULE, handle_call, [Port, "SSL"],
             "Updating listen port", ?LINE),
    updateListenPort(ssl, SSLOptions, Port, State);

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
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
handle_cast(enable, State) ->
    self() ! nextWorker,
    {noreply, State#state{disabled = false}};
handle_cast(disable, State) ->
    {noreply, State#state{disabled = true}};
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
handle_info(_Info, State = #state{disabled = true}) ->
    {noreply, State};
handle_info(nextWorker, State = #state{ssl            = SSLConfig,
                                       protocolModule = Module,
                                       listenSocket   = ListenSocket,
                                       allowedIps     = AllowedIps}) ->

    Worker = ePortAcceptor:start(Module,
                                 ListenSocket,
                                 AllowedIps,
                                 SSLConfig),
    {noreply, State#state{worker = Worker}};
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
terminate(Reason, #state{protocolModule = Module, listenSocket = LSocket}) ->
    eLog:log(debug, ?MODULE, terminate, [Reason, LSocket],
             "Shutting down...", ?LINE, Module),
    (catch gen_tcp:close(LSocket)),
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
getIpAddress(Socket) ->
    case catch inet:peername(Socket) of
        {ok, {Address, _}} ->
            inet_parse:ntoa(Address);
        _ ->
            case catch ssl:peername(Socket) of
                {ok, {Address, _}} ->
                    inet_parse:ntoa(Address);
                _ ->
                    %% No address of client was detected.
                    undefined
            end
    end.

updateListener(State = #state{worker = Worker}) when is_pid(Worker) ->
    erlang:exit(Worker, kill),
    self() ! nextWorker,
    {reply, ok, State#state{worker = undefined}};
updateListener(State) ->
    self() ! nextWorker,
    {reply, ok, State}.



updateListenPort(Protocol,SSLOptions,Port,State) when is_integer(Port) ->
    Protocol:close(State#state.listenSocket),
    case catch Protocol:listen(Port, ?TCPOptions++SSLOptions) of
        {ok, ListenSocket} ->
            {reply, ok, State#state{listenSocket = ListenSocket}};
        Reason ->
            eLog:log(error, ?MODULE, handle_cast, [Reason, Port],
                     "Failed to open port... shutting down.", ?LINE,
                     State#state.protocolModule),
            {stop, normal, State}
    end.
