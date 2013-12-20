#!/bin/sh
cd ../..
erl -pa examples/simple -pa ebin/ -pa lib/*/ebin/ -pa ../../ebin/ -boot start_sasl \
    -s exat \
    -http_port 7778 \
    -start simple_pingagent $*

    # -eval "t:t(mtp)." \
