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
-define(EXPAND_FLAG, true).
-record(state,{}).

%% TODO gen_server and gen_event
%% TODO add filtering for graphs?
%% TODO make this into an app

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
  Behaviour = lists:keyfind(behaviour, 3, ParsedFile),
  Behavior = lists:keyfind(behavior, 3, ParsedFile),
  Reply = case Behaviour of
            {attribute, _, _, gen_server} ->
              parse_gen_server(ParsedFile);
            {attribute, _, _, gen_fsm} ->
              parse_gen_fsm(ParsedFile);
            {attribute, _, _, gen_statem} ->
              parse_gen_statem(ParsedFile);
            {attribute, _, _, gen_event} ->
              parse_gen_event(ParsedFile);
            false ->
              case Behavior of
                {attribute, _, _, gen_server} ->
                  parse_gen_server(ParsedFile);
                {attribute, _, _, gen_fsm} ->
                  parse_gen_fsm(ParsedFile);
                {attribute, _, _, gen_statem} ->
                  parse_gen_statem(ParsedFile);
                {attribute, _, _, gen_event} ->
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

parse_gen_statem(TokenList) ->
  Type = gen_statem,
  Fun =
    fun({function, _, init, 1, Clauses}, Acc) ->
          parse_function_add_edges(Clauses, init, [async], Type, Acc);
       ({function, _, handle_event = FnName, 4, Clauses}, Acc) ->
          parse_function_add_node_and_edges(Clauses, FnName, [], Type, Acc);
       ({function, _, FnName, 3, Clauses}, Acc) ->
          parse_function_add_node_and_edges(Clauses, FnName, [], Type, Acc);
       (_Other, {OldNodes, OldEdges, OldAllStates}) ->
          {OldNodes, OldEdges, OldAllStates}
    end,
    Graph = generic_parse(TokenList, Fun),
    {parsed, Type, Graph}.

parse_gen_fsm(TokenList) ->
  Type = gen_fsm,
  Fun =
    fun({function, _, init, 1, Clauses}, Acc) ->
          parse_function_add_edges(Clauses, init, [async], Type, Acc);
       ({function, _, handle_event, 3, Clauses}, Acc) ->
          parse_function_add_states(Clauses, handle_event, [async, allstate], Type, Acc);
       ({function, _, handle_sync_event, 4, Clauses}, Acc) ->
          parse_function_add_states(Clauses, handle_sync_event, [sync, allstate], Type, Acc);
       ({function, _, handle_info, 3, Clauses}, Acc) ->
          parse_function_add_states(Clauses, handle_info, [async, allstate, info], Type, Acc);
       ({function, _, FnName, 2, Clauses}, Acc) ->
          %io:format("CLAUSES ~w for ASYNC FUNCTION ~w~n", [Clauses, FnName]),
          parse_function_add_node_and_edges(Clauses, FnName, [async], Type, Acc);
       ({function, _, FnName, 3, Clauses}, Acc) ->
          %io:format("CLAUSES ~w for SYNC FUNCTION ~w~n", [Clauses, FnName]),
          parse_function_add_node_and_edges(Clauses, FnName, [sync], Type, Acc);
       (_Other, {OldNodes, OldEdges, OldAllStates}) ->
            {OldNodes, OldEdges, OldAllStates}
    end,
    Graph = generic_parse(TokenList, Fun),
    {parsed, Type, Graph}.

generic_parse(TokenList, Fun) ->
  Graph = digraph:new(),
  {States, Edges, AllStateEdges} = lists:foldl(Fun, {[init, terminate],[],[]}, TokenList),
  {ExpAllStates, Expansion} = case ?EXPAND_FLAG of
				true ->
					{States, States};
				false ->
					{["*" | States], ["*"]}
			      end,
  NewAllStateEdges = expand_allstates(AllStateEdges, Expansion),%States),
  %io:format("~p~n", [NewAllStateEdges]),
  lists:foreach(fun(Vertex) ->
                  V = digraph:add_vertex(Graph),
                  digraph:add_vertex(Graph, V, Vertex)
                end, lists:usort(ExpAllStates)),
  lists:foreach(fun(#edge{vertex1 = From, vertex2 = To, edge_data = Data}) ->
                  digraph:add_edge(Graph, get_vertex(Graph, From),
                                   get_vertex(Graph,To), Data);
                   (_) -> ok
                end, remove_dups(Edges ++ NewAllStateEdges)),
  Graph.

parse_gen_event(_TokenList) ->
  {error, not_supported}.

parse_function_add_states(Clauses, FnName, Options, Type,
                          {OldNodes, OldEdges, OldAllStates}) ->
  case parse_function(Clauses, FnName, Options, Type) of
    {error, _Reason} ->
      {OldNodes, OldEdges, OldAllStates};
    NewEdges ->
      {OldNodes, OldEdges, OldAllStates ++ NewEdges}
  end.

parse_function_add_edges(Clauses, FnName, Options, Type,
                          {OldNodes, OldEdges, OldAllStates}) ->
  case parse_function(Clauses, FnName, Options, Type) of
    {error, _Reason} ->
      {OldNodes, OldEdges, OldAllStates};
    NewEdges ->
      {OldNodes, OldEdges ++ NewEdges, OldAllStates}
  end.
parse_function_add_node_and_edges(Clauses, FnName, Options, Type,
                                  {OldNodes, OldEdges, OldAllStates}) ->
  case parse_function(Clauses, FnName, Options, Type) of
    {error, _Reason} ->
      {OldNodes, OldEdges, OldAllStates};
    NewEdges ->
      {[FnName|OldNodes], OldEdges ++ NewEdges, OldAllStates}
  end.

parse_function(Clauses, FnName, Options, Type) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
               [parse_function_clause(Clause, FnName, Options, Type) | AccIn]
              end, [], Clauses),
  FlatEdges = lists:flatten(Edges),
  %io:format("For function ~p~n", [FnName]),
  % lists:foreach(fun(E) ->
  %       io:format(">>> ~p~n", [E])
  %   end, FlatEdges),

  case lists:all(fun (El) -> {error, bad_transition} == El end, Edges) of
    false ->
      FlatEdges;
    true ->
      {error, not_a_state}
  end.

parse_function_clause({clause, _Line, Args, Guards, Body},
                      init, _Options, Type) ->
  Fun = fun({ok, NextState, init}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            vertex1 = init,
            vertex2 = NextState,
            edge_data =
            #edge_data{
              event = "",
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = [async]}
          }
        end,
  map_parse_func(Fun, init, Body, Type);
parse_function_clause({clause, _Line, [Event | Args], Guards, Body},
                      handle_info, Options, gen_fsm) ->
  Fun = fun({ok, NextState, RetType}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            vertex1 = handle_info,
            vertex2 = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = [RetType|Options]}
          }
        end,
  map_parse_func(Fun, handle_info, Body, gen_fsm);
parse_function_clause({clause, _Line, [_EventType, State, Event | Args], Guards, Body},
                      handle_event, Options, gen_statem) ->
  Fun = fun({ok, NextState, RetType}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            vertex1 = State,
            vertex2 = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = [RetType|Options]}
          }
        end,
  map_parse_func(Fun, handle_info, Body, gen_fsm);
parse_function_clause({clause, _Line, [Event | Args], Guards, Body},
                      FnName, Options, gen_fsm) ->
  Fun = fun({ok, NextState, _}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            vertex1 = FnName,
            vertex2 = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = Options}
          }
        end,
  map_parse_func(Fun, FnName, Body, gen_fsm);
parse_function_clause({clause, _Line, [_Call, Event | Args], Guards, Body},
                      FnName, Options, gen_statem) ->
  Fun = fun({ok, NextState, _}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          #edge{
            vertex1 = FnName,
            vertex2 = NextState,
            edge_data =
            #edge_data{
              event = PrettyEvent,
              args = PrettyArgs,
              pattern = Args,
              guard = PrettyGuards,
              code = PrettyBody,
              attributes = Options}
          }
        end,
  map_parse_func(Fun, FnName, Body, gen_statem).

%process_body({ok. PrevState, NextState, }) ->

map_parse_func(Fun, State, Body, Type) ->
  case eval_return(State, parse_body(Body), Type, []) of
    {error, _Reason} ->
      {error, bad_transition};
    List ->
      lists:map(Fun, List)
  end.

parse_body(Body) ->
  parse_body(Body, []).

parse_body([Statement|Rest], Acc) ->
  parse_body(Rest, Acc ++ [parse_statement(Statement)]);
parse_body([], Acc) ->
%%   io:format("~p~n", [Acc]),
  lists:flatten([Acc]).

parse_statement({tuple, Line, Elems}) ->
  {tuple, Line, Elems};
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


eval_return(State, [ReturnVal|Rest], gen_fsm, Acc) ->
  case eval_tuple(ReturnVal) of
    {ok, {ok, NextState, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, init}|Acc]);
    {ok, {ok, NextState, _Data, _Timeout}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, init}|Acc]);
    {ok, {stop, _Reason}} ->
      eval_return(State, Rest, gen_fsm, [{ok, terminate, init}|Acc]);
    {ok, {reply, _Reply, NextState, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, sync}|Acc]);
    {ok, {reply, _Reply, NextState, _Data, _Timeout}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, sync}|Acc]);
    {ok, {stop, _Reason, _Reply, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, terminate, sync}|Acc]);
    {ok, {next_state, NextState, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, async}|Acc]);
    {ok, {next_state, NextState, _Data, _Timeout}} ->
      eval_return(State, Rest, gen_fsm, [{ok, NextState, async}|Acc]);
    {ok, {stop, _Reason, _Data}} ->
      eval_return(State, Rest, gen_fsm, [{ok, terminate, async}|Acc]);
    _Other ->
      eval_return(State, Rest, gen_fsm, Acc)
  end;
eval_return(State, [ReturnVal|Rest], gen_statem, Acc) ->
  case eval_tuple(ReturnVal) of
    {ok, {ok, NextState, _Data}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, init}|Acc]);
    {ok, {ok, NextState, _Data, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, init}|Acc]);
    {ok, ignore} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, init}|Acc]);

    {ok, stop} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop, _Reason}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop, _Reason, _NewData}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop_and_reply, _Reason, _Replies}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);
    {ok, {stop_and_reply, _Reason, _Replies, _NewData}} ->
      eval_return(State, Rest, gen_statem, [{ok, terminate, async}|Acc]);

    {ok, {next_state, NextState, _Data}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, async}|Acc]);
    {ok, {next_state, NextState, _Data, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, NextState, async}|Acc]);
    {ok, {keep_state, _NewData}} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    {ok, {keep_state, _NewData, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    {ok, keep_state_and_data} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    {ok, {keep_state_and_data, _Actions}} ->
      eval_return(State, Rest, gen_statem, [{ok, State, async}|Acc]);
    _Other ->
      eval_return(State, Rest, gen_statem, Acc)
  end;
eval_return(_, [], _, []) ->
  {error, badreturn};
eval_return(_, [], _, Acc) ->
  Acc.


eval_tuple({tuple, Line, Elements})->
  ClearElements = lists:map(fun(Element) ->
%%     io:format("NOW PROCESSING: ~p~n", [Element]),
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

expand_allstates(Edges, States) ->
  ClearStates = lists:delete(init, lists:delete(terminate, States)),
  lists:foldl(fun(Edge, Acc) ->
    #edge{vertex1 = FnName, vertex2 = To, edge_data = Data} = Edge,
    StateIndex = case FnName of
                   handle_event ->  1;
                   handle_sync_event -> 2;
                   handle_info -> 1
                 end,
    case To of
      '@var' ->
        case lists:nth(StateIndex, Data#edge_data.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++ lists:map(fun(State) ->
              #edge{vertex1 = StateFrom, vertex2 = State, edge_data = Data}
            end, ClearStates);
          _ ->
            Acc ++ lists:map(fun(State) ->
              #edge{vertex1 = State, vertex2 = State, edge_data = Data}
            end, ClearStates)
        end;
      Other ->
        case lists:nth(StateIndex, Data#edge_data.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++
              [#edge{vertex1 = StateFrom, vertex2 = Other, edge_data = Data}];
          _ ->
            Acc ++ lists:map(fun(State) ->
              #edge{vertex1 = State, vertex2 = Other, edge_data = Data}
            end, ClearStates)
        end
    end
  end, [], Edges).

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
