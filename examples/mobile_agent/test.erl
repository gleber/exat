.erl-module(test).


-export([test_run/1, test_migration/1, test_run_paralel/1, test_migration_p/1]). % API


test_run(Max)->
    {ok, File} = file:open("time_run"++integer_to_list(Max)++".csv", [write]),
    test_run1(File, Max),
    file:close(File).

test_run_paralel(Max)->
    {ok, File} = file:open("time_run_p"++integer_to_list(Max)++".csv", [write]),
    test_run2(File, Max).

test_run1(_, 0) ->
    ok;
test_run1(File, N) ->
    {T, _} = timer:tc(mobile_pingagent, start, [N]),
    io:format(File, "~p;~p;~p~n", [N, T,(round(erlang:memory(total)/1048576))]),
    test_run1(File, N-1).

test_run2(File, 0) ->
    ok;
test_run2(File, N) ->
    spawn(fun() -> start_agent(File, N) end),
    test_run2(File, N-1).
start_agent(File, N) ->
    {T, _} = timer:tc(mobile_pingagent, start, [N]),
    io:format(File, "~p;~p;~p~n", [N, T,(round(erlang:memory(total)/1048576))]).

test_migration(Max) ->
    AgentPids = [{N, mobile_pingagent:start(N)} || N <- lists:seq(1, Max)],
    {ok, File} = file:open("time_mig_p"++integer_to_list(Max)++".csv", [write]),
    [migrate_agent(File, N, Pid) ||
        {N, {ok, Pid}} <- AgentPids],
    file:close(File).

test_migration_p(Max) ->
    AgentPids = [{N, mobile_pingagent:start(N)} || N <- lists:seq(1, Max)],
    {ok, File} = file:open("time_mig"++integer_to_list(Max)++".csv", [write]),
    [spawn(fun() -> migrate_agent(File, N, Pid) end) ||
        {N, {ok, Pid}} <- AgentPids].

migrate_agent(File, N, Pid) ->
    {T, _} = timer:tc(gen_server, call, 
                      [Pid, {mobility, send_me, <<"http://ubuntu:7779">>}]),
    io:format(File, "~p;~p;~p~n", [N, T,(round(erlang:memory(total)/1048576))]).
