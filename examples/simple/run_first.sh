#!/bin/sh
cd ../..
erl -pa examples/simple -pa ebin/ -pa lib/*/ebin/ -boot start_sasl \
    -s exat_app \
    -http_port 7778 \
    -start simple_pingagent $*
