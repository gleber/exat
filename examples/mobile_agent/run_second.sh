#!/bin/sh
cd ../..
erl -pa examples/mobile_agent -pa ebin/ -pa lib/*/ebin/ -pa ../../ebin/ -boot start_sasl \
    -s exat \
    -http_port 7779 \
    -config examples/mobile_agent/mobility \
    -m_tcp_port 1806 \
    -start mobile_pingeragent $*
    #-sname b

    # -eval "t:t(agent)." \
    # -eval "t:t(simple_pingeragent)." \
