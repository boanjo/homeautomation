all: get-deps compile

get-deps:
	./rebar get-deps

compile:
	./rebar compile

run:
	erl -pa ebin/ deps/*/ebin/ -config etc/cowboy.config -config etc/homeautomation.config -config etc/app.config -eval "application:start(sasl)" -eval "error_logger:logfile({open, \"log/error_logger.txt\"})" -eval "application:start(txrx)" -eval "application:start(homeautomation)" -eval "nitrogen_sup:start_link()" -sname boan -setcookie hej -detached

attach:
	erl -sname attach -setcookie hej -remsh boan@raspberrypi

stop:
	erl -sname attach -setcookie hej -eval "rpc:call(boan@raspberrypi, homeautomation_server, stop, [])" -eval "init:stop()"


plugins:
	@(export PATH=`pwd`/`echo erts-*/bin`:$$PATH;escript do-plugins.escript)



copy-static:
	@(cp -r deps/nitrogen_core/www/* priv/static//nitrogen/)

