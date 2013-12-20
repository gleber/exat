-module(exat_test).

-include_lib("exat/include/acl.hrl").
-include_lib("exat/include/fipa_ontology.hrl").

-include_lib("eunit/include/eunit.hrl").

shutdown_test() ->
    exat:start(),
    ?assertEqual(true, is_process_alive(whereis(agent_registry))),
    exat:stop(),
    t:ut(),
    ?assertEqual(undefined, whereis(agent_registry)),
    ok.



noop_test() ->
    ?assertMatch([_|_], noop_agent:module_info()),

    exat:start(),
    {ok, Pinger} = agent:start_link(pinger, noop_agent, [{state, pinger}]),
    ?assertEqual(true, erlang:is_process_alive(Pinger)),
    ?assertEqual(Pinger, whereis(pinger)),
    ok = agent:stop(Pinger),
    application:stop(exat),

    ok.

addresses_test() ->
    exat:start(),
    ?assertMatch([_], mtp:addresses()),
    [Address] = mtp:addresses(),
    ?assertEqual(true, is_binary(Address)),
    exat:stop().


pingpong_test() ->
    Mod = noop_agent,
    meck:new(Mod, [passthrough]),
    meck:expect(Mod, handle_acl, fun(#aclmessage{}, State) ->
                                         {noreply, State}
                                 end),
    exat:start(),
    {ok, Ponger} = agent:start_link(ponger, noop_agent, [{state, ponger}]),
    Addrs = mtp:addresses(),
    Dest = #'agent-identifier'{name = <<"ponger">>,
                               addresses = Addrs},
    PingMsg = #aclmessage{sender = undefined,
                          receiver = Dest, content = <<"ping">>},
    Resp = acl:query_ref(PingMsg),
    ok = agent:stop(Ponger),
    exat:stop(),
    ?assertEqual(true, meck:validate(Mod)),
    meck:unload(Mod),
    ok.
