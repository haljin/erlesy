-module(graph_builder).

-behaviour(gen_server).

%% API
-export([start_link/0,
  parse_file/2]).

-export([test/0, test/1, parse_statement/1]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("types.hrl").
-define(SERVER, ?MODULE).

-record(state, {}).

%% @TODO gen_server and gen_event
%% @TODO add filtering for graphs?
%% @TODO make this into an app

%%%===================================================================
%%% API
%%%===================================================================
 parse_file(File, Includes) ->
   gen_server:call(?MODULE, {parse_file, File, Includes}).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

test() ->
  test(1).

test(1) ->
  {ok, File} = file:open("output", [write]),
  start_link(),
  {_,_,Graph} = parse_file("test/example_fsm.erl", []),
  io:format(File, "~s~n", [dot:digraph_to_dot("test", Graph)]),
  file:close(File).

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
init([]) ->
  {ok, #state{}}.

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
handle_call({parse_file, File, Includes}, _From, State) ->
  {ok, ParsedFile} = epp:parse_file(File, Includes, []),
  Reply = case lists:keyfind(behaviour, 3, ParsedFile) of
            {attribute, _, behaviour, gen_server} ->
              parse_gen_server(ParsedFile);
            {attribute, _, behaviour, gen_fsm} ->
              parse_gen_fsm(ParsedFile);
            {attribute, _, behaviour, gen_event} ->
              parse_gen_event(ParsedFile);
            false ->
              case lists:keyfind(behavior, 3, ParsedFile) of
                {attribute, _, behavior, gen_server} ->
                  parse_gen_server(ParsedFile);
                {attribute, _, behavior, gen_fsm} ->
                  parse_gen_fsm(ParsedFile);
                {attribute, _, behavior, gen_event} ->
                  parse_gen_event(ParsedFile);
                false ->
                  {error, not_otp}
              end
          end,
  {reply, Reply, State};
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
parse_gen_server(_TokenList) ->
  {error, not_supported}.

parse_gen_fsm(TokenList) ->
  Graph = digraph:new(),
  Edges = lists:foldl(
    fun(Token, OldEdges) ->
      case Token of
        {function, _, FunctionName, ParamCount, Clauses} ->
          case parse_gen_fsm__function(Clauses, FunctionName, ParamCount) of
            {error, _Reason} ->
              OldEdges;
            NewEdges ->
              OldEdges ++ NewEdges
          end;
        _Other ->
          OldEdges
      end
    end,
    [], TokenList),

  Verts = lists:usort([#vertex{name=init, type=state},
           #vertex{name=terminate, type=state} |
           get_verts_from_edges(Edges, [])]),

  NewEdges = expand_allstates(Edges, [V || V=#vertex{name = State} <- Verts, State =/= handle_info andalso
                                                                             State =/= handle_sync_event andalso
                                                                             State =/= handle_event andalso
                                                                             State =/= init andalso
                                                                             State =/= terminate]),

  lists:foreach(fun(Vertex) ->
    V = digraph:add_vertex(Graph),
    digraph:add_vertex(Graph, V, Vertex)
                end, lists:usort(Verts)),

  lists:foreach(fun(#edge{vertex1 = FromV, vertex2 = ToV, edge_data = Data}) ->
    digraph:add_edge(Graph, get_vertex(Graph, FromV), get_vertex(Graph, ToV), Data)
                end, remove_dups(TotalE=
                              [E||E=#edge{vertex1 = #vertex{name=FromName}, vertex2 = #vertex{name=ToName}}
                                <-NewEdges++Edges, FromName =/= '@var' andalso ToName =/= '@var'])),
  io:format("WTF? ~p~n", [TotalE]),
  {parsed, gen_fsm, Graph}.

parse_gen_event(_TokenList) ->
  {error, not_supported}.

get_verts_from_edges([], Acc) ->
  Acc;
get_verts_from_edges([#edge{vertex1 = V1, vertex2 = V2} | Rest], Acc) ->
  get_verts_from_edges(Rest, [V1, V2 | Acc]).


parse_gen_fsm__function(Clauses, F=handle_event, _) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
    [parse_gen_fsm__function_clause(Clause, F, [async, allstate]) | AccIn]
                      end, [], Clauses),
  parse_gen_fsm__function_evaluate(Edges);
parse_gen_fsm__function(Clauses, F=handle_sync_event, _) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
    [parse_gen_fsm__function_clause(Clause, F, [sync, allstate]) | AccIn]
                      end, [], Clauses),
  parse_gen_fsm__function_evaluate(Edges);
parse_gen_fsm__function(Clauses, F=handle_info, _) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
    %% Not sure why we want to have info in options here, but maybe Pawel can tell me
    [parse_gen_fsm__function_clause(Clause, F, [async, allstate, info]) | AccIn]
                      end, [], Clauses),
  parse_gen_fsm__function_evaluate(Edges);
parse_gen_fsm__function(Clauses, F=init, _) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
    [parse_gen_fsm__function_clause(Clause, F, [async, init]) | AccIn]
                      end, [], Clauses),
  parse_gen_fsm__function_evaluate(Edges);
parse_gen_fsm__function(_, terminate, _) ->
  {error, non_state};
parse_gen_fsm__function(Clauses, F, 2) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
    [parse_gen_fsm__function_clause(Clause, F, [async]) | AccIn]
                      end, [], Clauses),
  parse_gen_fsm__function_evaluate(Edges);
parse_gen_fsm__function(Clauses, F, 3) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
    [parse_gen_fsm__function_clause(Clause, F, [sync]) | AccIn]
                      end, [], Clauses),
  parse_gen_fsm__function_evaluate(Edges);
parse_gen_fsm__function(_, _, _) ->
  {error, non_state}.


parse_gen_fsm__function_evaluate(Edges) ->
  case lists:any(fun (Element) -> Element=={error, bad_transition} end, Edges) of
    false ->
      lists:flatten(Edges);
    true ->
      {error, no_transition_found}
  end.
%%parse_gen_fsm__function_clause(Clause, Function, Options) ->
%%  io:format("CLAUSE:~w~nFUN:~w~nOpts:~w~n~n", [Clause, Function, Options]),
%%  {error, bad_transition};


parse_gen_fsm__function_clause({clause, _Line, FunctionArgs=[FunctionEvent | _Args],
                                FunctionGuards, FunctionBody}, FunctionName, Options) ->
  case eval_return(parse_gen_fsm__body(FunctionBody), []) of
    {error, _Reason} ->
      {error, bad_transition};
    {conditional, ConditionalFlows} ->
      lists:map(
        fun({FromVertex, FromType, FromLine, ToVertex, ToType, ToLine, ConditionalPattern,
             ConditionalGuards, ConditionalBody})->
          PrettyConditionalPattern = erl_pp:exprs(ConditionalPattern),
          PrettyConditionalGuards = lists:map(fun(G) -> erl_pp:guard(G) end, ConditionalGuards),
          PrettyConditionalBody = erl_pp:exprs(ConditionalBody),
          #edge{
            vertex1 = #vertex{name=FromVertex, type=FromType, line=FromLine},
            vertex2 = #vertex{name=ToVertex, type=ToType, line=ToLine},
            edge_data = #edge_data{pattern = PrettyConditionalPattern,
                                   guard = PrettyConditionalGuards,
                                   code = ok}%PrettyConditionalBody}
            }
        end, ConditionalFlows);
      List ->
        PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, FunctionGuards),
        PrettyBody = erl_pp:exprs(FunctionBody),
        PrettyEvent = erl_pp:expr(FunctionEvent),
        PrettyArgs = erl_pp:exprs(FunctionArgs),
      lists:map(
        fun({ok, NextState, RetType}) ->
          #edge{
            vertex1 = #vertex{name=FunctionName, type=state},
            vertex2 = #vertex{name=NextState, type=state},
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = FunctionArgs,
              guard = PrettyGuards,
              code = ok,%PrettyBody,
              attributes = remove_dups([RetType|Options])}
          }
        end, List)
  end.


%process_body({ok. PrevState, NextState, }) ->


parse_gen_fsm__body(Body) ->
  parse_gen_fsm__body(Body, []).

parse_gen_fsm__body([Statement|Rest], Acc) ->
  parse_gen_fsm__body(Rest, Acc ++ [parse_statement(Statement)]);
parse_gen_fsm__body([], Acc) ->
  lists:flatten([Acc]).

parse_statement({tuple, Line, Elems}) ->
  {tuple, Line, Elems};
parse_statement(S={'case', _Line, _Op, _Clauses}) ->
  %%io:format("Nick wants: ~w~n", [{'case', Line, Op, Clauses}]),
  lists:map(fun(Index) ->
    parse_statement(element(Index, S))
            end, lists:seq(1, size(S)));
parse_statement(Statement) when is_tuple(Statement) ->
  lists:map(fun(Index) ->
    parse_statement(element(Index, Statement))
  end, lists:seq(1, size(Statement)));
parse_statement(Statement) when is_list(Statement) ->
  lists:map(fun(Index) ->
    parse_statement(lists:nth(Index, Statement))
  end, lists:seq(1, length(Statement)));
parse_statement(_Statement) ->
  [].


eval_return([ReturnVal|Rest], Acc) ->
  case eval_tuple(ReturnVal) of
    {ok, {ok, NextState, _Data}} ->
      eval_return(Rest, [{ok, NextState, init}|Acc]);
    {ok, {ok, NextState, _Data, _Timeout}} ->
      eval_return(Rest, [{ok, NextState, init}|Acc]);
    {ok, {stop, _Reason}} ->
      eval_return(Rest, [{ok, terminate, init}|Acc]);
    {ok, {reply, _Reply, NextState, _Data}} ->
      eval_return(Rest, [{ok, NextState, sync}|Acc]);
    {ok, {reply, _Reply, NextState, _Data, _Timeout}} ->
      eval_return(Rest, [{ok, NextState, sync}|Acc]);
    {ok, {stop, _Reason, _Reply, _Data}} ->
      eval_return(Rest, [{ok, terminate, sync}|Acc]);
    {ok, {next_state, NextState, _Data}} ->
      eval_return(Rest, [{ok, NextState, async}|Acc]);
    {ok, {next_state, NextState, _Data, _Timeout}} ->
      eval_return(Rest, [{ok, NextState, async}|Acc]);
    {ok, {stop, _Reason, _Data}} ->
      eval_return(Rest, [{ok, terminate, async}|Acc]);
    _Other ->
      eval_return(Rest, Acc)
  end;
eval_return([], []) ->
  {error, badreturn};
eval_return([], Acc) ->
  Acc.


eval_tuple({tuple, Line, Elements})->
  ClearElements = lists:map(fun(Element) ->
    case Element of
      {atom, RLine, Atom} ->
        {atom, RLine, Atom};
      _Other ->
        {atom, 0, '@var'}
    end
  end, Elements),
  ClearTuple = {tuple, Line, ClearElements},
  {value, Val, _} = erl_eval:expr(ClearTuple, []), %Bindings),
  {ok, Val};
eval_tuple(_Other)->
  {error, not_a_tuple}.

expand_allstates(Edges, ClearStates) ->
  %%ClearStates = lists:keydelete(init, #vertex.name, lists:keydelete(terminate, #vertex.name, States)),
  ClearEdges = [E || #edge{vertex1 = #vertex{name = V1}} = E <- Edges,
                V1 == handle_event orelse V1 == handle_sync_event orelse V1 == handle_info],
  lists:foldl(fun(Edge, Acc) ->
    #edge{vertex1 = #vertex{name=FnName}, vertex2 = #vertex{name=To}, edge_data = Data} = Edge,
    StateNameIndex = case FnName of
                   handle_event ->  2;
                   handle_sync_event -> 3;
                   handle_info -> 2
                 end,
    case To of
      '@var' ->
        case lists:nth(StateNameIndex, Data#edge_data.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++ lists:map(fun(#vertex{name=State}) ->
              #edge{vertex1 = #vertex{name=StateFrom, type = state},
                    vertex2 = #vertex{name=State, type = state}, edge_data = Data}
                             end, ClearStates);
          _ ->
            Acc ++ lists:map(fun(#vertex{name=State}) ->
              #edge{vertex1 = #vertex{name=State, type = state},
                    vertex2 = #vertex{name=State, type = state}, edge_data = Data}
                             end, ClearStates)
        end;
      Other ->
        case lists:nth(StateNameIndex, Data#edge_data.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++
            [#edge{vertex1 = #vertex{name=StateFrom, type = state},
                   vertex2 = #vertex{name=Other, type = state}, edge_data = Data}];
          _ ->
            Acc ++ lists:map(fun(State) ->
              #edge{vertex1 = #vertex{name=State, type = state},
                    vertex2 = #vertex{name=Other, type = state}, edge_data = Data}
                             end, ClearStates)
        end
    end
  end, [], ClearEdges).

get_vertex(Graph, Label) ->
  Vertices = digraph:vertices(Graph),
  find_label(Label, Vertices, Graph).

find_label(Label, [V|Rest], Graph) ->
  case digraph:vertex(Graph, V) of
    {V, Label} ->
      V;
    _ ->
      find_label(Label, Rest, Graph)
  end;
find_label(_, [], _) ->
  {error, no_label}.

remove_dups([])    -> [];
remove_dups([H|T]) -> [H | [X || X <- remove_dups(T), X /= H]].


abstract_print(Form) ->
  erl_prettypr:format(erl_syntax:form_list([Form])).
