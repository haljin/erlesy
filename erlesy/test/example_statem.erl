-module(example_statem).

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([ init/1
        , callback_mode/0
        , terminate/3
        , code_change/4
        ]).

-export([ state_1/3
        , state_2/3
        , case_state/3
        ]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> state_functions.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
init([]) ->
  {ok, state_1, 2}.

state_1(_EventType, e1, Data) ->
  {next_state, state_2, Data};
state_1(_EventType, e2, Data) ->
  {stop, normal, Data};
state_1(_EventType, e3, Data) ->
  {keep_state, Data}.

state_2(_EventType, e4, Data) ->
  {next_state, state_1, Data};
state_2(_EventType, e5, Data) ->
  {next_state, case_state, Data}.

case_state(_EventType, {e6, Param}, Data) ->
  do_some_things,
  do_some_more_things,
  case Param =:= 123 of
    true ->
      case Data =:= 2 of
        true ->
          if
            Data == 2 ->
              {keep_state, Data};
            Data == 3 ->
              {next_state, state_2, Data}
          end;
        false ->
          {stop, blah, Data}
      end;

    false ->
      {keep_state, Data}
  end.

terminate(_TermEvent, _StateName, _State) ->
  ok.

code_change(_CodeChangeEvent, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
