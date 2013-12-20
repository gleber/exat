#!/bin/sh
cd ../..
erl +P 1001000 -pa examples/mobile_agent -pa ebin/ -pa lib/*/ebin/ -pa ../../ebin/ \
    -kernel error_logger 'silent' \
    -boot start_sasl \
    -s exat \
    -http_port 7778 \
    -config examples/mobile_agent/mobility \
    -name a@192.168.1.103 \
    -setcookie ABCD \
    -start mobile_pingagent $*

    # -eval "t:t(mtp)." \
