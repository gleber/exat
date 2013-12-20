all: deps fast

fast: rebar
	./rebar compile

deps: rebar
	./rebar get-deps
	./rebar compile

clean: rebar
	./rebar clean

clean-deps:
	rm -rf lib

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

rebar:
	wget -q http://cloud.github.com/downloads/basho/rebar/rebar
	chmod u+x rebar

test:
	./rebar skip_deps=true eunit

eunit: test
