#!/bin/sh
cd ../..
erl -pa examples/mobile_agent -pa ebin/ -pa lib/*/ebin/ -pa ../../ebin/ -boot start_sasl \
    -s exat_app \
    -http_port 7778 \
    -config examples/mobile_agent/mobility \
    -sname a \
    -start mobile_pingagent $*

    # -eval "t:t(mtp)." \
