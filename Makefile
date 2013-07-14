.PHONY: deps dev test

compile: deps
	./rebar compile

deps: rebar
	./rebar get-deps

clean: rebar
	./rebar clean

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

test:
	./rebar eunit
