-module(graph_builder).

-behaviour(gen_server).

%% API
-export([start_link/0,
  parse_file/2]).

-export([test/0, test2/0]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-include("types.hrl").
-define(SERVER, ?MODULE).

-record(state, {                                     l
}).

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
  {ok, File} = file:open("output", [write]),
  start_link(),
  {_,_,Graph} = parse_file("mobile.erl", []),
  io:format(File, "~s~n", [dot:digraph_to_dot("test", Graph)]),
  file:close(File).

test2() ->
  {ok, File} = file:open("output2", [write]),
  start_link(),
  {_,_,Graph} = parse_file("vm_control.erl", []),
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
  {States, Edges, AllStateEdges} = lists:foldl(
    fun(Token, {OldNodes, OldEdges, OldAllStates}) ->
      case Token of
        {function, _, init, 1, Clauses} ->
          case parse_function(Clauses, init, [async], gen_fsm) of
            {error, _Reason} ->
              {OldNodes, OldEdges, OldAllStates};
            NewEdges ->
              {OldNodes, OldEdges ++ NewEdges, OldAllStates}
          end;
        {function, _, handle_event, 3, Clauses} ->
          case parse_function(Clauses, handle_event, [async, allstate], gen_fsm) of
            {error, _Reason} ->
              {OldNodes, OldEdges, OldAllStates};
            NewEdges ->
              {OldNodes, OldEdges, OldAllStates ++ NewEdges}
          end;
        {function, _, handle_sync_event, 4, Clauses} ->
          case parse_function(Clauses, handle_sync_event, [sync, allstate], gen_fsm) of
            {error, _Reason} ->
              {OldNodes, OldEdges, OldAllStates};
            NewEdges ->
              {OldNodes, OldEdges, OldAllStates ++ NewEdges}
          end;
        {function, _, handle_info, 3, Clauses} ->
          case parse_function(Clauses, handle_info, [async, allstate, info], gen_fsm) of
             {error, _Reason} ->
               {OldNodes, OldEdges, OldAllStates};
             NewEdges ->
               {OldNodes, OldEdges, OldAllStates ++ NewEdges}
           end;
        {function, _, FnName, 2, Clauses} ->
           case parse_function(Clauses, FnName, [async], gen_fsm) of
             {error, _Reason} ->
               {OldNodes, OldEdges, OldAllStates};
             NewEdges ->
              {[FnName|OldNodes], OldEdges ++ NewEdges, OldAllStates}
             end;
        {function, _, FnName, 3, Clauses} ->
          case parse_function(Clauses, FnName, [sync], gen_fsm) of
            {error, _Reason} ->
              {OldNodes, OldEdges, OldAllStates};
            NewEdges ->
              {[FnName|OldNodes], OldEdges ++ NewEdges, OldAllStates}
          end;
        _Other ->
          {OldNodes, OldEdges, OldAllStates}
      end
    end, {[init, terminate],[],[]}, TokenList),
  %io:format("~p~n", [States]),
%%   lists:foreach(fun({edge, From, To, Data}) ->
%%     io:format("~p~n", [{edge, From, To, Data#graph_edge.event}])
%%   end, AllStateEdges),
  NewAllStateEdges = expand_allstates(AllStateEdges, States),
  %io:format("~p~n", [NewAllStateEdges]),
  lists:foreach(fun(Vertex) ->
                  V = digraph:add_vertex(Graph),
                  digraph:add_vertex(Graph, V, Vertex)
                end, lists:usort(States)),
  lists:foreach(fun({edge, From, To, Data}) ->
                  %io:format("~p~n", [{edge, From, To}]),
                  digraph:add_edge(Graph, get_vertex(Graph, From), get_vertex(Graph,To), Data)
                end, Edges ++ NewAllStateEdges),
  {parsed, gen_fsm, Graph}.

parse_gen_event(_TokenList) ->
  {error, not_supported}.

parse_function(Clauses, FnName, Options, Type) ->
  Edges = lists:foldl(fun(Clause, AccIn) ->
               [parse_clause(Clause, FnName, Options, Type) | AccIn]
              end, [], Clauses),
  io:format("For function ~p~n", [FnName]),
%%   lists:foreach(fun(E) ->
%%     case E of
%%       {edge, From, To, Data} ->
%%         io:format("~p ~s ~n", [{edge, From, To}, Data#graph_edge.event]);
%%       Other ->
%%         io:format("~p~n", [Other])
%%     end end, Edges),
  case lists:any(fun (El) -> {error, bad_transition} == El end, Edges) of
    false ->
      lists:flatten(Edges);
    true ->
      {error, not_a_state}
  end.

parse_clause({clause, _Line, Args, Guards, Body}, init, _Options, Type) ->
  case eval_return(parse_body(Body), Type, []) of
    {error, _Reason} ->
      {error, bad_transition};
    List ->
      lists:map(
        fun({ok, NextState, init}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyArgs = erl_pp:exprs(Args),
          {edge, init, NextState, #graph_edge{event = "", args = PrettyArgs, pattern = Args, guard = PrettyGuards,
          code = PrettyBody, attributes = [async]}}
        end, List)
  end;
parse_clause({clause, _Line, [Event | Args], Guards, Body}, handle_info, Options, Type) ->
  case eval_return(parse_body(Body), Type, []) of
    {error, _Reason} ->
      {error, bad_transition};
    List ->
      lists:map(
        fun({ok, NextState, RetType}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          {edge, handle_info, NextState, #graph_edge{event = PrettyEvent, args = PrettyArgs, pattern = Args, guard = PrettyGuards,
          code = PrettyBody, attributes = [RetType|Options]}}
        end, List)
  end;
parse_clause({clause, _Line, [Event | Args], Guards, Body}, FnName, Options, Type) ->
  case eval_return(parse_body(Body), Type, []) of
    {error, _Reason} ->
      {error, bad_transition};
    List ->
      lists:map(
        fun({ok, NextState, _}) ->
          PrettyGuards = lists:map(fun(G) -> erl_pp:guard(G) end, Guards),
          PrettyBody = erl_pp:exprs(Body),
          PrettyEvent = erl_pp:expr(Event),
          PrettyArgs = erl_pp:exprs(Args),
          {edge, FnName, NextState, #graph_edge{event = PrettyEvent, args = PrettyArgs, pattern = Args, guard = PrettyGuards,
          code = PrettyBody, attributes = Options}}
        end, List)
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


eval_return([ReturnVal|Rest], gen_fsm, Acc) ->
  case eval_tuple(ReturnVal) of
    {ok, {ok, NextState, _Data}} ->
      eval_return(Rest, gen_fsm, [{ok, NextState, init}|Acc]);
    {ok, {ok, NextState, _Data, _Timeout}} ->
      eval_return(Rest, gen_fsm, [{ok, NextState, init}|Acc]);
    {ok, {stop, _Reason}} ->
      eval_return(Rest, gen_fsm, [{ok, terminate, init}|Acc]);
    {ok, {reply, _Reply, NextState, _Data}} ->
      eval_return(Rest, gen_fsm, [{ok, NextState, sync}|Acc]);
    {ok, {reply, _Reply, NextState, _Data, _Timeout}} ->
      eval_return(Rest, gen_fsm, [{ok, NextState, sync}|Acc]);
    {ok, {stop, _Reason, _Reply, _Data}} ->
      eval_return(Rest, gen_fsm, [{ok, terminate, sync}|Acc]);
    {ok, {next_state, NextState, _Data}} ->
      eval_return(Rest, gen_fsm, [{ok, NextState, async}|Acc]);
    {ok, {next_state, NextState, _Data, _Timeout}} ->
      eval_return(Rest, gen_fsm, [{ok, NextState, async}|Acc]);
    {ok, {stop, _Reason, _Data}} ->
      eval_return(Rest, gen_fsm, [{ok, terminate, async}|Acc]);
    _Other ->
      eval_return(Rest, gen_fsm, Acc)
  end;
eval_return([], _, []) ->
  {error, badreturn};
eval_return([], _, Acc) ->
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
    {edge, FnName, To, Data} = Edge,
    StateIndex = case FnName of
                   handle_event ->  1;
                   handle_sync_event -> 2;
                   handle_info -> 1
                 end,
    case To of
      '@var' ->
        case lists:nth(StateIndex, Data#graph_edge.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++ lists:map(fun(State) ->
              {edge, StateFrom, State, Data}
            end, ClearStates);
          _ ->
            Acc ++ lists:map(fun(State) ->
              {edge, State, State, Data}
            end, ClearStates)
        end;
      Other ->
        case lists:nth(StateIndex, Data#graph_edge.pattern) of
          {atom, _line, StateFrom} ->
            Acc ++
              [{edge, StateFrom, Other, Data}];
          _ ->
            Acc ++ lists:map(fun(State) ->
              {edge, State, Other, Data}
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
