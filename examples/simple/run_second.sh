#!/bin/sh
cd ../..
erl -pa examples/simple -pa ebin/ -pa lib/*/ebin/ -pa ../../ebin/ -boot start_sasl \
    -s exat_app \
    -http_port 7779 \
    -start simple_pingeragent $*

    # -eval "t:t(agent)." \
    # -eval "t:t(simple_pingeragent)." \
