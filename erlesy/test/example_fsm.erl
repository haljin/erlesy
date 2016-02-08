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

-record(state, {}).

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

state_1(an_event, State) ->
  {next_state, state_2, State};
state_1(end_event, State) ->
  {stop, normal, State}.

state_1(sync_event, _From, State) ->
  {reply, ok, state_1, State}.

state_2(an_event, State) ->
  {next_state, state_1, State};
state_2(another_event, State) ->
  {next_state, case_state, State}.

case_state({another_event, Param}, State) ->
  case Param of
    true -> {next_state, state_1, State};
    false -> {next_state, case_state, State}
  end.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
