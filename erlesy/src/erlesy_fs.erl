%%% ===================================================================
%%% @doc
%%% The file server responsible for writing and reading from files
%%% @end
%%% ===================================================================
-module(erlesy_fs).

-behaviour(gen_server).

%% API
-export([start_link/0,
         write_output/2]).

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

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

write_output(Name, Output) ->
  gen_server:call(?SERVER, {write_output, Name, Output}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({write_output, FileName, Output}, _From, State) ->
  {ok, File} = file:open(FileName, [write]),
  file:write(File, Output),
  file:close(File),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
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

