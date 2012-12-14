#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname myweb_server_dev \
    -s myweb_server \
    -s reloader \
	-mnesia dir '"db/dev"'
