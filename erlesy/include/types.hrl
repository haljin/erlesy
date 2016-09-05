-record(graph, {graph_ref, name, type}).


-record(vertex, {name,   %% Some text representation
                 type,   %% case, if, state
                 line}). %% used to differentiate if and case in the graphs


-record(edge, {vertex1,
               vertex2,
               edge_data}).

-record(edge_data, {event,
                    pattern,
                    args,
                    guard,
                    code,
                    attributes}).