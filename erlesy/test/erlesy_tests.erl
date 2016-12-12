-module(erlesy_tests).
-include_lib("eunit/include/eunit.hrl").

create_graph_test_() ->
    %% Since there are currently some async calls, we need to manually loop, and wait...
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        % Gen Fsm
        {"Create fsm into Json", fun create_graph_gen_fsm_json/0},
        {"Create fsm into Dot", fun create_graph_gen_fsm_dot/0},

        % Gen StateM
        {"Create statem into Json", fun create_graph_gen_statem_json/0},
        {"Create statem into Dot", fun create_graph_gen_statem_dot/0}
     ]
    }.

create_graph_gen_fsm_json() ->
    io:format("~p~n", [file:get_cwd()]),
    ?assert(filelib:is_file("example_fsm.beam")),
    ?assert(ok == otp_parser:create_graph("example_fsm.erl", [], json)),
    ?assert(ok == wait_unitl_file_complete("example_fsm.json")),
    ?assert(example_fsm_json() == file:read_file("example_fsm.json")).

create_graph_gen_fsm_dot() ->
    io:format("~p~n", [file:get_cwd()]),
    ?assert(filelib:is_file("example_fsm.beam")),
    ?assert(ok == otp_parser:create_graph("example_fsm.erl", [], dot)),
    ?assert(ok == wait_unitl_file_complete("example_fsm.txt")),
    ?assert(example_fsm_dot() == file:read_file("example_fsm.txt")).

create_graph_gen_statem_json() ->
    io:format("~p~n", [file:get_cwd()]),
    ?assert(filelib:is_file("example_statem.beam")),
    ?assert(ok == otp_parser:create_graph("example_statem.erl", [], json)),
    ?assert(ok == wait_unitl_file_complete("example_statem.json")),
    ?assert(example_statem_json() == file:read_file("example_statem.json")).

create_graph_gen_statem_dot() ->
    io:format("~p~n", [file:get_cwd()]),
    ?assert(filelib:is_file("example_statem.beam")),
    ?assert(ok == otp_parser:create_graph("example_statem.erl", [], dot)),
    ?assert(ok == wait_unitl_file_complete("example_statem.txt")),
    ?assert(example_statem_dot() == file:read_file("example_statem.txt")).

%%------------------------------------------------------------------------------------

setup() ->
    otp_parser_app:start(),

    ?assert(undefined /= whereis(otp_parser) ),
    ?assert(undefined /= whereis(graph_builder) ),
    ?assert(undefined /= whereis(erlesy_fs) ),
    stuff.

cleanup(stuff) ->
    ok.

wait_unitl_file_complete(ExpectedFile) ->
    wait_loop(ExpectedFile, 100, 20).

wait_loop(ExpectedFile, Duration, 0) ->
    {error, not_completed_done_waiting};
wait_loop(ExpectedFile, Duration, Times) when Times > 0 ->
    timer:sleep(Duration),
    case filelib:is_file(ExpectedFile) of
        true ->
            ok;
        false ->
            wait_loop(ExpectedFile, Duration, Times-1)
    end.

%%------------------------------------------------------------------------------------

example_fsm_json() ->
    {ok,
        <<"{\"vertices\":[{\"name\":\"state_1\"},"
        "{\"name\":\"case_state\"},"
        "{\"name\":\"terminate\"},"
        "{\"name\":\"state_2\"},"
        "{\"name\":\"init\"}],\"edges\":[{\"source\":\"state_2\",\"target\":\"state_2\",\"label\":\"_Info\"},"
        "{\"source\":\"case_state\",\"target\":\"case_state\",\"label\":\"{e6,Param}\"},"
        "{\"source\":\"case_state\",\"target\":\"terminate\",\"label\":\"{e6,Param}\"},"
        "{\"source\":\"state_2\",\"target\":\"case_state\",\"label\":\"e5\"},"
        "{\"source\":\"case_state\",\"target\":\"state_2\",\"label\":\"{e6,Param}\"},"
        "{\"source\":\"case_state\",\"target\":\"case_state\",\"label\":\"_HandleSyncEvent\"},"
        "{\"source\":\"state_1\",\"target\":\"state_1\",\"label\":\"e3\"},"
        "{\"source\":\"state_2\",\"target\":\"state_2\",\"label\":\"_HandleSyncEvent\"},"
        "{\"source\":\"state_1\",\"target\":\"state_1\",\"label\":\"_HandleEvent\"},"
        "{\"source\":\"case_state\",\"target\":\"case_state\",\"label\":\"_Info\"},"
        "{\"source\":\"init\",\"target\":\"state_1\",\"label\":\"\"},"
        "{\"source\":\"state_1\",\"target\":\"state_1\",\"label\":\"_HandleSyncEvent\"},"
        "{\"source\":\"state_1\",\"target\":\"state_1\",\"label\":\"_Info\"},"
        "{\"source\":\"state_2\",\"target\":\"state_2\",\"label\":\"_HandleEvent\"},"
        "{\"source\":\"state_2\",\"target\":\"state_1\",\"label\":\"e4\"},"
        "{\"source\":\"case_state\",\"target\":\"case_state\",\"label\":\"_HandleEvent\"},"
        "{\"source\":\"state_1\",\"target\":\"terminate\",\"label\":\"e2\"},"
        "{\"source\":\"state_1\",\"target\":\"state_2\",\"label\":\"e1\"}]}">>
    }.

example_fsm_dot() ->
    {ok,
        <<"digraph example_fsm { \n"
        "rankdir=LR;\n"
        "edge [fontsize=10];\n"
        "node [shape=circle];\n"
        "ranksep = 2;\n"
        "nodesep = 0.5\n"
        "state_2->state_2[label=\"_Info\"]\n"
        "case_state->case_state[label=\"{e6,Param}\"]\n"
        "case_state->terminate[label=\"{e6,Param}\"]\n"
        "state_2->case_state[label=\"e5\"]\n"
        "case_state->state_2[label=\"{e6,Param}\"]\n"
        "case_state->case_state[label=\"_HandleSyncEvent\"]\n"
        "state_1->state_1[label=\"e3\"]\n"
        "state_2->state_2[label=\"_HandleSyncEvent\"]\n"
        "state_1->state_1[label=\"_HandleEvent\"]\n"
        "case_state->case_state[label=\"_Info\"]\n"
        "init->state_1[label=\"\"]\n"
        "state_1->state_1[label=\"_HandleSyncEvent\"]\n"
        "state_1->state_1[label=\"_Info\"]\n"
        "state_2->state_2[label=\"_HandleEvent\"]\n"
        "state_2->state_1[label=\"e4\"]\n"
        "case_state->case_state[label=\"_HandleEvent\"]\n"
        "state_1->terminate[label=\"e2\"]\n"
        "state_1->state_2[label=\"e1\"]\n"
        "\n"
        "}">>
    }.

example_statem_json() ->
    {ok,
        <<"{\"vertices\":[{\"name\":\"state_1\"},"
        "{\"name\":\"case_state\"},"
        "{\"name\":\"terminate\"},"
        "{\"name\":\"state_2\"},"
        "{\"name\":\"init\"}],\"edges\":[{\"source\":\"case_state\",\"target\":\"case_state\",\"label\":\"{e6,Param}\"},"
        "{\"source\":\"case_state\",\"target\":\"terminate\",\"label\":\"{e6,Param}\"},"
        "{\"source\":\"state_2\",\"target\":\"case_state\",\"label\":\"e5\"},"
        "{\"source\":\"case_state\",\"target\":\"state_2\",\"label\":\"{e6,Param}\"},"
        "{\"source\":\"state_1\",\"target\":\"state_2\",\"label\":\"e1\"},"
        "{\"source\":\"init\",\"target\":\"state_1\",\"label\":\"\"},"
        "{\"source\":\"state_2\",\"target\":\"state_1\",\"label\":\"e4\"},"
        "{\"source\":\"state_1\",\"target\":\"state_1\",\"label\":\"e3\"},"
        "{\"source\":\"state_1\",\"target\":\"terminate\",\"label\":\"e2\"}]}">>
    }.

example_statem_dot() ->
    {ok,
        <<"digraph example_statem { \n"
        "rankdir=LR;\n"
        "edge [fontsize=10];\n"
        "node [shape=circle];\n"
        "ranksep = 2;\n"
        "nodesep = 0.5\n"
        "case_state->case_state[label=\"{e6,Param}\"]\n"
        "case_state->terminate[label=\"{e6,Param}\"]\n"
        "state_2->case_state[label=\"e5\"]\n"
        "case_state->state_2[label=\"{e6,Param}\"]\n"
        "state_1->state_2[label=\"e1\"]\n"
        "init->state_1[label=\"\"]\n"
        "state_2->state_1[label=\"e4\"]\n"
        "state_1->state_1[label=\"e3\"]\n"
        "state_1->terminate[label=\"e2\"]\n"
        "\n}">>
    }.
