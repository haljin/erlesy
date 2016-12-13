.PHONY: compile get-deps update-deps test clean deep-clean

compile: get-deps update-deps
	rebar compile

get-deps:
	rebar get-deps

update-deps:
	rebar update-deps

clean:
	rebar clean

deep-clean: clean
	rebar delete-deps

setup_dialyzer:
	dialyzer --build_plt --apps erts kernel stdlib runtime_tools syntax_tools deps/*/ebin ./ebin
	dialyzer --add_to_plt ebin

dialyzer: compile
	dialyzer ebin

analyze: checkplt
	rebar skip_deps=true dialyze

buildplt: setup_dialyzer
	rebar skip_deps=true build-plt

checkplt: buildplt
	rebar skip_deps=true check-plt
	
eunit:
	rebar skip_deps=true eunit