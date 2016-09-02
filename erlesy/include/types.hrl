-record(graph, {graph_ref, name, type}).

-record(edge, {vertex1, vertex2, edge_data}).
-record(edge_data, {event, pattern, args, guard, code, attributes}).