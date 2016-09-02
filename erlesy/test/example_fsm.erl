-module(example_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1,
  state_1/2,
  state_1/3,
  state_2/2,
  case_state/2,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {val=2}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([]) ->
  {ok, state_1, #state{}}.

state_1(e1, State) ->
  {next_state, state_2, State};
state_1(e2, State) ->
  {stop, normal, State}.

state_1(e3, _From, State) ->
  {reply, ok, state_1, State}.

state_2(e4, State) ->
  {next_state, state_1, State};
state_2(e5, State) ->
  {next_state, case_state, State}.

case_state({e6, Param}, State) ->
  do_some_things,
  do_some_more_things,
  case Param =:= 123 of
    true ->
      case State#state.val =:= 2 of
        true ->
          if
            State#state.val == 2 ->
              {next_state, case_state, State};
            State#state.val == 3 ->
              {next_state, state_2, State}
          end;
        false ->
          {stop, blah, State}
      end;

    false ->
      {next_state, case_state, State}
  end.

handle_event(_HandleEvent, StateName, State) ->
  case 1 == 1 of
    true ->
      {next_state, StateName, State};
    false ->
      {next_state, StateName, State}
  end.


handle_sync_event(_HandleSyncEvent, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_TermEvent, _StateName, _State) ->
  ok.

code_change(_CodeChangeEvent, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
