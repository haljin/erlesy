-module(otp_parser).

-behaviour(gen_server).

%% API
-export([start_link/0, create_graph/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_graph(FileName, IncludePaths, Mode)->
  gen_server:cast(?MODULE, {create, FileName, IncludePaths, Mode}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({create, FileName, IncludePaths, json}, State) ->
  {parsed, _, Digraph} = graph_builder:parse_file(FileName, IncludePaths),
  Output = erlesy_json_builder:encode(Digraph),
  erlesy_fs:write_output(filename:rootname(FileName) ++ erlesy_json_builder:extension(), Output),
  {noreply, State};
handle_cast({create, FileName, IncludePaths, dot}, State) ->
  {ok, File} = file:open(filename:rootname(FileName) ++ ".gv", [write]),
  {parsed, _, Digraph} = graph_builder:parse_file(FileName, IncludePaths),
  file:write(File, dot:digraph_to_dot(filename:rootname(FileName), Digraph)),
  file:close(File),
  {noreply, State};
handle_cast({create, FileName, IncludePaths, plantuml}, State) ->
  {ok, File} = file:open(filename:rootname(FileName) ++ ".txt", [write]),
  {parsed, _, Digraph} = graph_builder:parse_file(FileName, IncludePaths),
  file:write(File, dot:digraph_to_plantuml(filename:rootname(FileName), Digraph)),
  file:close(File),
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
