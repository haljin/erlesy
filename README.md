# erlesy

ErlEsy is a simple graphing tool for Erlang. It allows for automatic generation of state machine diagrams out of Erlang source files. It works by parsing an .erl source file and building a digraph out of it. The digraph can be then transformed into any format that will allow for its graphical representation.

## Building

ErlEsy can be build by rebar, simply invoke

```rebar get-deps
rebar compile```

## Running 

To run ErlEsy simply start the application by calling

`application:start(erlesy).`

Then you can create any file by using

```
-spec create_graph(string(), [string()], dot | json) -> ok.
otp_parser:create_graph(FileName, IncludeFiles, Mode).```

* FileName should be a path to the .erl file you want to graph
* IncludeFiles is a list of paths towards include files
* Mode is either dot or json

## Output and graphing

ErlEsy currently supports two forms of output, a DOT formatted file or a JSON file.

### DOT

DOT file can be used e.g. at http://www.webgraphviz.com/

### JSON

A simple visualization made with d3.js is provided with ErlEsy in erlesy_vis. To use it simply place your json in the root of erlesy_vis (Currently it has to be named example_fsm.json) and launch a web server (e.g. Python SimpleHTTPServer) in that directory.
