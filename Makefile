all: compile

deps:
	rebar get-deps

compile: deps
	rebar compile

eunit: compile
	rebar eunit skip_deps=true

clean:
	rebar clean
	rm -rf priv ebin .eunit deps
