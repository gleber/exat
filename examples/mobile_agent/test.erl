-module(test).


-export([test_run/1, test_migration/1]). % API


test_run(Max)->
    {ok, File} = file:open("time_run"++integer_to_list(Max)++".csv", [write]),
    test_run1(File, Max),
    file:close(File).

test_run1(_, 0) ->
    ok;
test_run1(File, N) ->
    {T, _} = timer:tc(mobile_pingagent, start, [N]),
    io:format(File, "~p;~p~n", [N, T]),
    test_run1(File, N-1).

test_migration(Max) ->
    AgentPids = [{N, mobile_pingagent:start(N)} || N <- lists:seq(1, Max)],
    {ok, File} = file:open("time_mig"++integer_to_list(Max)++".csv", [write]),
    [migrate_agent(File, N, Pid) ||
        {N, {ok, Pid}} <- AgentPids],
    file:close(File).

migrate_agent(File, N, Pid) ->
    {T, _} = timer:tc(gen_server, call, 
                      [Pid, {mobility, send_me, <<"http://192.168.1.100:7779">>}]),
    io:format(File, "~p;~p~n", [N, T]).
