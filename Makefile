all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

skip-deps: get-deps compile
	./rebar skip_deps=true

run:
	erl -pa ebin/ deps/*/ebin/ -config etc/cowboy.config -config etc/app.config -eval "application:start(tellstick)" -eval "application:start(homeautomation)" -eval "nitrogen_sup:start_link()"



plugins:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH;escript do-plugins.escript)



copy-static:
	@(cp -r deps/nitrogen_core/www/* priv/static//nitrogen/)

