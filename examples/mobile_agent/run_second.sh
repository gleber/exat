#!/bin/sh
cd ../..
erl -pa examples/mobile_agent -pa ebin/ -pa lib/*/ebin/ -pa ../../ebin/ -boot start_sasl \
    -s exat_app \
    -http_port 7779 \
    -config examples/mobile_agent/mobility \
    -sname b \
    -m_tcp_port 1806 \
    -start mobile_pingeragent $*

    # -eval "t:t(agent)." \
    # -eval "t:t(simple_pingeragent)." \
