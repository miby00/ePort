%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(waiterServer).
-behaviour(gen_server).


%%%===================================================================
%%% Exports
%%%===================================================================
%%% API
-export([start_link/0]).
-export([
         amountOfFoodEaten/1,
         getChopsticks/1,
         takeBackChopsticks/1,
         clientConnected/3,
         clientDisconnected/2
        ]).

%%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%%%===================================================================
%%% Macros and records
%%%===================================================================
-define(SERVER, ?MODULE).

%%% Server state
-define(state, #{chopsticks         => 5,
                 doorbellButton     => undefined,
                 philosophers       => [],
                 philosophers_eaten => []
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

amountOfFoodEaten(Id) ->
    FoodEaten = gen_server:call(?SERVER, {amountOfFoodEaten, Id}),
    io:format("Waiter: philosopher ~p has eaten ~p amount of "
              "food~n", [Id, FoodEaten]).

getChopsticks(EPortPid) ->
    gen_server:call(?SERVER, {getChopsticks, EPortPid}).

takeBackChopsticks(EPortPid) ->
    gen_server:cast(?SERVER, {takeBackChopsticks, EPortPid}).

clientConnected(EPortPid, _EListenerPid, _PeerHost) ->
    gen_server:cast(?SERVER, {clientConnected, EPortPid}).

clientDisconnected(EPortPid, _EListenerPid) ->
    gen_server:cast(?SERVER, {clientDisconnected, EPortPid}).


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
init(_) ->
    process_flag(trap_exit, true),
    {ok, SrvPid} = ePortListener:start_link(waiter, 19000),
    State = ?state#{doorbellButton := SrvPid},
    {ok, State}.

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
%% @end2
%%--------------------------------------------------------------------
handle_call({amountOfFoodEaten, Id}, _From,
            #{philosophers := Phils} = State) ->
    {EPortPid, Id} = lists:keyfind(Id, 2, Phils),
    FoodEaten = ePort:call(EPortPid, amountOfFoodEaten, [Id]),
    {reply, FoodEaten, State};
handle_call({getChopsticks, EPortPid}, _From,
            #{chopsticks         := Chopsticks,
              philosophers       := Phils,
              philosophers_eaten := Eaten} = State) ->
    {EPortPid, Id} = lists:keyfind(EPortPid, 1, Phils),
    io:format("Waiter: philosopher ~p want sticks ~n", [Id]),
    HasEaten = lists:member({EPortPid, Id},Eaten),
    {Reply, NewState}=
        case Chopsticks >= 2 of
            true  ->
                case HasEaten of
                    true ->
                        case length(Phils) == length(Eaten) of
                            true ->
                                io:format("Waiter: philosopher ~p got "
                                          "sticks, everyone has "
                                          "eaten~n",[Id]),
                                NewChopsticks = Chopsticks -2,
                                NewEaten = [{EPortPid,Id}],
                                {2, State#{
                                      chopsticks := NewChopsticks,
                                      philosophers_eaten := NewEaten}};
                            false ->
                                io:format("Waiter: philosopher ~p got no "
                                          " sticks, already ate~n", [Id]),
                                {0, State}
                        end;
                    false ->
                        io:format("Waiter: philosopher "
                                  "~p got sticks~n",[Id]),
                        NewChopsticks = Chopsticks -2,
                        NewHasEaten = [{EPortPid,Id}|Eaten],
                        {2, State#{chopsticks := NewChopsticks,
                                   philosophers_eaten := NewHasEaten}}
                end;
            false ->
                io:format("Waiter: philosopher "
                          "~p got no sticks, no sticks left~n",[Id]),
                {0, State}
        end,
    {reply, Reply, NewState};
handle_call(Request, _From, State) ->
    io:format("Waiter: Unexpected call msg \"~p\"~n", [Request]),
    {reply, ok, State}.

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
handle_cast({takeBackChopsticks, EPortPid},
            #{chopsticks := Chopsticks,
              philosophers := Phils} = State) ->
    {EPortPid, Id} = lists:keyfind(EPortPid, 1, Phils),
    io:format("Waiter: philosopher ~p gave back sticks ~n", [Id]),
    NewState = State#{chopsticks := (Chopsticks + 2)},
    {noreply, NewState};
handle_cast({clientConnected, EPortPid},
            #{philosophers := Phils} = State) ->
    Id = length(Phils)+1,
    NewState = State#{philosophers := [{EPortPid, Id} | Phils]},
    io:format("Waiter: philosopher ~p sits down at table~n", [Id]),
    {noreply, NewState};
handle_cast({clientDisconnected, EPortPid},
            #{philosophers := Phils} = State) ->
    {EPortPid, Id} = lists:keyfind(EPortPid, 1, Phils),
    NewPhils = lists:keydelete(EPortPid, 1, Phils),
    NewState = State#{philosophers := NewPhils},
    io:format("Waiter: philosopher ~p left table~n", [Id]),
    {noreply, NewState};
handle_cast(Msg, State) ->
    io:format("Waiter: Unexpected cast msg \"~p\"~n", [Msg]),
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
terminate(_Reason, _State) ->
    io:format("~nWaiter server: Going home for the day~n"),
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
