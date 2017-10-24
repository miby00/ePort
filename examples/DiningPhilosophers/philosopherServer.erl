%%%===================================================================
%%% @author Taddic <tommy.mattsson@gmail.com
%%% @copyright (C) 2017, Mikael Bylund
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Taddic
%%%===================================================================
-module(philosopherServer).
-behaviour(gen_server).


%%%===================================================================
%%% Exports
%%%===================================================================
%%% API
-export([start_link/2]).
-export([
         amountOfFoodEaten/1
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
-define(SERVER(Id), list_to_atom("philosopher"++integer_to_list(Id))).

%%% Server state
-define(state, #{id         => undefined,
                 chopsticks => 0,
                 waiter     => undefined,
                 food_eaten => 0}).

-define(TIMEOUT, rand:uniform(8000)+2000).

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
start_link(Id, Restaurant) ->
    gen_server:start_link({local,?SERVER(Id)},?MODULE,{Id,Restaurant},[]).

amountOfFoodEaten(Id) ->
    gen_server:call(?SERVER(Id), amountOfFoodEaten).

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
init({Id, Restaurant}) ->
    process_flag(trap_exit, true),
    Door = 19000,   %% The port to connect on
    {ok, ClientPid} = ePort:start_link(philosopher, Restaurant, Door),
    State = ?state#{id := Id,
                    waiter := ClientPid},
    {ok, State, ?TIMEOUT}.

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
handle_call(amountOfFoodEaten, _From,
            #{food_eaten := FoodEaten} = State) ->
    {stop, normal, FoodEaten, State};
handle_call(Request, _From, #{id := Id} = State) ->
    io:format("Philosopher ~p: Unexpected call msg \"~p\"~n",
              [Id, Request]),
    {reply, ok, State, ?TIMEOUT}.

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
handle_cast(Msg, #{id := Id} = State) ->
    io:format("Philosopher ~p: Unexpected cast msg \"~p\"~n",
              [Id, Msg]),
    {noreply, State, ?TIMEOUT}.

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
handle_info(timeout, #{id := Id,
                       chopsticks := Chopsticks,
                       waiter := Waiter,
                       food_eaten := FoodEaten} = State) ->
    Timeout = ?TIMEOUT,
    NewState =
        case Chopsticks of
            0 ->
                case ePort:call(Waiter, getChopsticks, []) of
                    0 ->
                        io:format("Philosopher ~p: thinking for ~p~n",
                                  [Id, Timeout]),
                        State;
                    NewChopsticks ->
                        io:format("Philosopher ~p: eating for ~p~n",
                                  [Id, Timeout]),
                        State#{chopsticks := NewChopsticks,
                               food_eaten := (FoodEaten + Timeout)}
                end;
            2 ->
                io:format("Philosopher ~p: thinking for ~p~n",
                          [Id, Timeout]),
                ePort:cast(Waiter, takeBackChopsticks, []),
                State#{chopsticks := 0}
        end,
    {noreply, NewState, Timeout};
handle_info(Info, #{id := Id} = State) ->
    io:format("Philosopher ~p: Unexpected info msg \"~p\"~n",
              [Id, Info]),
    {noreply, State, ?TIMEOUT}.

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
terminate(_Reason, #{id := Id,
                     waiter := Waiter}) ->
    io:format("~nPhilosopher ~p: Finished eating, going home~n", [Id]),
    ePort:stop(Waiter).

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
