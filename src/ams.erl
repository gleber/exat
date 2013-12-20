%%
%% ams.erl
%%
%% ----------------------------------------------------------------------
%% Copyright (c) 2003-04, Corrado Santoro <csanto@diit.unict.it>
%% Department of Computer and Telecommunication Engineering,
%% University of Catania, Italy. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%%
%% * Redistributions in binary form must reproduce the above copyright
%%   notice, this list of conditions and the following disclaimer in the
%%   documentation and/or other materials provided with the distribution.
%%
%% * Neither the name of Corrado Santoro nor the name of University of Catania
%%   may be used to endorse or promote products derived from this
%%   software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
%%

-module(ams).

-export([start_link/0]).

-export([de_register_agent/1, get_registered_agents/0, get_registered_agents/1,
         register_agent/1, register_agent/2, register_agent/3, get_migration_parameters/2]).

-behaviour(agent).

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2,
         terminate/2]).

-include_lib("exat/include/acl.hrl").

-include_lib("exat/include/fipa_ontology.hrl").

-record(state, {agent_registry,
                rules_server,

                migration_requests=[],
                migration_id=0,
                nodes=[]
               }).

handle_acl(#aclmessage{speechact = 'REQUEST',
                       ontology = ?FIPA_AGENT_MANAGEMENT,
                       protocol = <<"fipa-request">>} = Message,
           State) ->
    F = fun(Content) ->
                ContentReply = prepare_reply(Content),
                spawn(fun () -> acl:reply(Message, 'INFORM', ContentReply) end)
        end,

    case Message#aclmessage.content of
        L when is_list(L) ->
            lists:foreach(F, L);
        C -> F(C)
    end,
    {noreply, State};

handle_acl(#aclmessage{speechact = 'QUERY-REF', content = <<"migration", Agent/binary>>} = Message,
           #state{nodes = Nodes} = State) ->
    [_ , Host] = re:split(Agent, "@"),
    Node = binary_to_atom(re:replace(Host, ":", "@", [{return, binary}]), utf8),
    {Content, State1} = case net_adm:ping(Node) of
                            pong ->
                                %%io:format("in erlang cluster~n"),
                                MyNode = atom_to_binary(node(), utf8),
                                State0 = case lists:member(Node, Nodes) of
                                             false -> %%refresh gproc, join to cluster
                                                 catch exit(whereis(gproc_dist), kill),
                                                 State#state{nodes = [Node | Nodes]};
                                             _ -> State
                                         end,
                                {<<"migration", "erl", MyNode/binary>>, State0};
                            _ ->
                                %%io:format("not in cluster"),
                                Port = list_to_binary(integer_to_list(proc_mobility:get_tcp_server_port())),
                                {<<"migration", "tcp", Port/binary>>, State}
                        end,
    acl:reply(Message, 'INFORM', Content),
    {noreply, State1};

handle_acl(#aclmessage{speechact = 'INFORM', content = <<"migration", Parameters/binary>>, 'conversation-id' = ConvId}, #state{migration_requests=Requests}=State) ->
    ConvIdI = list_to_integer(binary_to_list(ConvId)),
    Caller = proplists:get_value(ConvIdI, Requests),
    gen_server:reply(Caller, Parameters),
    {noreply, State#state{migration_requests=proplists:delete(ConvIdI, Requests)}};
handle_acl(Message, State) ->
    io:format("unknown message!~p ~n", [Message]),
    {noreply, State}.

prepare_reply(Content = #action{'1' = #'get-description'{}}) ->
    logger:log('AMS', "get-description"),
    APService = #'ap-service'{name =
                                  "fipa.mts.mtp.http.std",
                              type = "fipa.mts.mtp.http.std",
                              addresses = mtp:addresses()},
    APDescription = #'ap-description'{'name' = exat:current_platform(),
                                      'ap-services' = [APService]},
    #result{'0' = Content, '1' = [APDescription]};

prepare_reply(Content = #action{'1' = #search{'0' = #'ams-agent-description'{}}}) ->
    logger:log('AMS', "search ams-agent-description"),
    Agents = ams:get_registered_agents(),
    Descriptions = [#'ams-agent-description'{name = X,
                                             ownership = "NONE",
                                             state = "active"}
                    || X <- Agents],
    #result{'0' = Content, '1' = Descriptions};
prepare_reply(Content = #action{'1' = #register{name = Agent}}) ->
    AgentName = Agent#'agent-identifier'.name,
    Addrs = Agent#'agent-identifier'.addresses,
    seresye:assert(agent_registry, {agent, AgentName, nil, Addrs}),
    #result{'0' = Content, '1' = Agent}.

%%====================================================================
%% Func: start_link/0
%% Returns: {ok, Pid}.
%%====================================================================
start_link() ->
    agent:start_link(ams, ams, [{behaviour, ams}, no_register]).

init(ams, _Params) ->
    process_flag(trap_exit, true),
    logger:start('AMS'),
    logger:log('AMS', {"Ancestors: ~p", [get('$ancestors')]}),
    logger:log('AMS', {"exat_sup: ~p", [whereis(exat_sup)]}),
    {ok, Pid} = seresye:start(agent_registry),
    AgentRegistry = ets:new(agent_registry, [bag, public]),

    logger:log('AMS', "Staring AMS."),
    %% ontology_service:register_codec(?FIPA_AGENT_MANAGEMENT,
    %%                                 fipa_ontology_sl_codec),
    {ok, #state{agent_registry=AgentRegistry,
                rules_server = Pid}}.


handle_call({register, Agent, Pid, Addrs}, _From, #state{agent_registry=Registry} = State) ->
    Ref = erlang:monitor(process, Pid),
    ets:insert(Registry, {Agent, Ref, Addrs}),
    {reply, ok, State};

handle_call({migration, AgentName, Destination}, From, #state{migration_id = MId, migration_requests=Requests} = State) ->
    Dest = #'agent-identifier'{name = <<"ams">>, addresses=[Destination]},
    AgentNameB = atom_to_binary(AgentName, utf8),
    acl:query_ref(#aclmessage{sender = ams,
                              receiver = Dest,
                              'conversation-id' = list_to_binary(integer_to_list(MId)),
                              content = <<"migration", AgentNameB/bitstring>>}),
    {noreply, State#state{migration_id = MId+1, migration_requests = [{MId,From} | Requests]}};

handle_call({registered_agents, AgentName}, _From, #state{agent_registry = Registry} = State) ->
    {reply, ets:match_object(Registry,{AgentName, '_','_'}), State};

handle_call({deregister, AgentName}, _From, #state{agent_registry = Registry} = State) ->
    {reply, ets:delete(Registry, AgentName), State};

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call, Call}, State}.

handle_cast(Cast, State) ->
    {reply, {error, unknown_cast, Cast}, State}.

handle_info({'DOWN', Ref, process, _Pid, _} = Msg, #state{agent_registry=Registry} = State) ->
    %%seresye:retract_match(agent_registry, {agent, '_', Ref, '_'}),
    %%io:format("~p ~p ~p~n", [Ref, ets:tab2list(Registry), ets:match_object(Registry, {'_', Ref, '_'})]),
    ets:match_delete(Registry, {'_', Ref, '_'}),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{rules_server = Pid} = _State) ->
    ok = seresye:stop(Pid),
    ok.

%%====================================================================
%% Func: register_agent/1
%% Returns: ok.
%%====================================================================
register_agent(AgentName) ->
    register_agent(AgentName, self()).

%%====================================================================
%% Func: register_agent/2
%% Returns: ok.
%%====================================================================
register_agent(AgentName, Pid) when is_pid(Pid) ->
    register_agent(AgentName, mtp:addresses(), Pid);
register_agent(AgentName, Addresses) ->
    register_agent(AgentName, Addresses, self()).

register_agent(AgentName, Addresses, Pid) ->
    agent:call(ams, {register, AgentName, Pid, Addresses}).

%%====================================================================
%% Func: de_register_agent/1
%% Returns: ok.
%%====================================================================
de_register_agent(AgentName) ->
    %%seresye:retract_match(agent_registry, {agent, AgentName, '_', '_'}).
    agent:call(ams, {deregister, AgentName}).

%%====================================================================
%% Func: get_registered_agents/0
%% Returns: [string()].
%%====================================================================
get_registered_agents() ->
    get_registered_agents('_').

get_registered_agents(AgentName) ->
                                                %AgentList = seresye:query_kb(agent_registry,
                                                %                             {agent, AgentName, '_', '_'}),
    AgentList = agent:call(ams, {registered_agents, AgentName}),
    [#'agent-identifier'{name = X, addresses = Y} || {X, _, Y} <- AgentList].
%%PlatformName = exat:current_platform(),
%%[lists:flatten([atom_to_list(X), "@", PlatformName])
%% || X <- AgentLocalNames].


get_migration_parameters(Agent, Destination) ->
    agent:call(ams, {migration, Agent, Destination}).
