#!/bin/sh


help()
{
  echo ""
  echo "usage:  exat.sh platform_name flags"
  echo ""
  echo "Flags:"
  echo "  -noshell             -- do not start erlang shell"
  echo "  -http_port PORT      -- use given http port instead of 7779 (default)"
  echo "  -start app1,..,appN  -- start the given agent applications"
  echo ""
  echo ""

  echo "eXAT, an erlang eXperimental Agent Tool"
  echo "ERESYE, an ERlang Expert SYstem Engine"
  echo "Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)"
  echo "Copyright (C) 2005-07 Francesca Gangemi (francesca@erlang-consulting.com)"
  echo "This program comes with ABSOLUTELY NO WARRANTY; for details type '$0 --license'."
  echo "This is free software, and you are welcome to redistribute it"
  echo "under certain conditions; type '$0 --license' for details."

  echo ""
  exit 1
}

if [ $# -gt 0 ]
then
    # if [ "$1" == "--license" ];
    # then
    #     less `dirname $0`/LICENSE.txt
    #     exit
    # fi
    # if [ "$1" == "-license" ];
    # then
    #     less `dirname $0`/LICENSE.txt
    #     exit
    # fi
    # if [ "$1" == "--help" ];
    # then
    #     help
    #     exit
    # fi
    # if [ "$1" == "-help" ];
    # then
    #     help
    #     exit
    # fi
    PLATFORM_NAME=$1
    shift
    erl -pa ebin/ -sname $PLATFORM_NAME -boot start_sasl -s exat_app $*
else
    help
fi
