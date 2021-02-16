-module(basic_server).

-behaviour(gen_server).

-export([ getPort/1
        , info/7
        , start_link/0
        , subscribe/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================
getPort(Server) ->
    gen_server:call(Server, getPort).

info(ProtMod, LogLevel, Module, Function, Args, ErrorDesc, LineNumber) ->
    Fmt =
        [ LogLevel
        , ProtMod
        , Module
        , Function
        , LineNumber
        , ErrorDesc
        , Args
        ],
    ct:log("~w (~w) ~w:~w L~w~n~s~nArgs:~p~n", Fmt).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], [{debug, [trace]}]).

subscribe(EPid) ->
    gen_server:call(?SERVER, {subscribe, EPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    {ok, #{}}.

handle_call(getPort, From, #{}) ->
    {noreply, #{wait => From}};
handle_call(getPort, _From, #{epid := EPid} = State) ->
    {reply, EPid, State};
handle_call({subscribe, EPid}, _From, State) ->
    case State of
        #{wait := From} -> gen_server:reply(From, EPid);
        _ -> ok
    end,
    {reply, ok, #{epid => EPid}}.

handle_cast(_Request, State) ->
    {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
