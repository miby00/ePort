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
         doAccept/5,
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
-define(TCPOptions, [{reuseaddr, true},{active, false}, {packet, 4}]).
-define(Timeout, 30000).

-record(state, {allowedIps = all,
                disabled = false,
                listenSocket,
                port,
                protocolModule,
                ssl = false
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
                        port           = LPort,
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
                        port           = LPort,
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
handle_call({updateIPAllowed, IPAllowed}, _From,
            State = #state{ssl = false}) when is_list(IPAllowed) ->
    eLog:log(debug, ?MODULE, handle_call, [IPAllowed],
             "Updating allowed IPs", ?LINE),
    updateListener(gen_tcp, [], State#state{allowedIps = IPAllowed});
handle_call({updateIPAllowed, IPAllowed}, _From,
            State = #state{ssl = {true, SSLOptions}}) when
      is_list(IPAllowed) ->
    eLog:log(debug, ?MODULE, handle_call, [IPAllowed, "SSL"],
             "Updating allowed IPs", ?LINE),
    updateListener(ssl, SSLOptions, State#state{allowedIps = IPAllowed});

handle_call({updateModulesAllowed, Modules}, _From,
            State = #state{ssl = false}) when is_list(Modules) ->
    eLog:log(debug, ?MODULE, handle_call, [Modules],
             "Updating allowed protocol modules", ?LINE),
    updateListener(gen_tcp, [], State#state{protocolModule = Modules});
handle_call({updateModulesAllowed, Modules}, _From,
            State = #state{ssl = {true,SSLOptions}}) when
            is_list(Modules) ->
    eLog:log(debug, ?MODULE, handle_call, [Modules, "SSL"],
             "Updating allowed protocol modules", ?LINE),
    updateListener(ssl, SSLOptions, State#state{protocolModule = Modules});

handle_call({updateListenPort, Port}, _From,
            State = #state{ssl = false}) when is_integer(Port) ->
    eLog:log(debug, ?MODULE, handle_call, [Port],
             "Updating listen port", ?LINE),
    updateListener(gen_tcp, [], State#state{port = Port});
handle_call({updateListenPort, Port}, _From,
            State = #state{ssl = {true,SSLOptions}}) when is_integer(Port) ->
    eLog:log(debug, ?MODULE, handle_call, [Port, "SSL"],
             "Updating listen port", ?LINE),
    updateListener(ssl, SSLOptions, State#state{port = Port});

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

    spawn(?MODULE, doAccept, [self(), Module,
                              ListenSocket, AllowedIps, SSLConfig]),
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
    IP = getIpAddress(Socket),
    {lists:member(IP, AllowedIps), IP};
allowedIp(Socket,  _) ->
    IP = getIpAddress(Socket),
    {true, IP}.

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

updateListener(Protocol, SSLOptions, State) ->
    Protocol:close(State#state.listenSocket),
    case catch Protocol:listen(State#state.port, ?TCPOptions++SSLOptions) of
        {ok, ListenSocket} ->
            {reply, ok, State#state{listenSocket = ListenSocket}};
        Reason ->
            eLog:log(error, ?MODULE, handle_cast, [Reason, State#state.port],
                     "Failed to open port... shutting down.", ?LINE,
                     State#state.protocolModule),
            {stop, normal, State}
    end.
