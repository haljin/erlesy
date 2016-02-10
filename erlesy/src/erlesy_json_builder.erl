-module(erlesy_json_builder).

-include("types.hrl").
%% API
-export([encode/1,
         extension/0]).


encode(Digraph) ->
  Vertices = [parse_vertex(Vertex, Digraph) || Vertex <- digraph:vertices(Digraph)],
  Edges = [parse_edge(Edge, Digraph) || Edge <- digraph:edges(Digraph)],
  Output = [{vertices, Vertices}, {edges, Edges}],
  jsx:encode(Output).

extension() ->
  ".json".

parse_vertex(Vertex, Digraph) ->
  {_, VLabel} = digraph:vertex(Digraph, Vertex),
  [{name, VLabel}].

parse_edge(Edge, Digraph) ->
  {_, V1, V2, EdgeInfo} = digraph:edge(Digraph, Edge),
  {_, V1Label} = digraph:vertex(Digraph, V1),
  {_, V2Label} = digraph:vertex(Digraph, V2),
  EdgeLabel = case EdgeInfo#graph_edge.guard of
                [] ->
                  io_lib:format("~s", [EdgeInfo#graph_edge.event]);
                Guard ->
                  io_lib:format("~s [~s]", [EdgeInfo#graph_edge.event, Guard])
              end,
  TrimmedLabel = list_to_binary(lists:flatten(EdgeLabel)),
  [{source, V1Label}, {target, V2Label}, {label, TrimmedLabel}].

