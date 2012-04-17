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

-export([action/2, event/2, extends/0, pattern/2,
         request_proc/4]).

-export([de_register_agent/1, get_registered_agents/0,
         register_agent/1, start_link/0]).

-include("acl.hrl").

-include("fipa_ontology.hrl").

extends() -> nil.

pattern(Self, request) ->
    [#aclmessage{speechact = 'REQUEST',
                 ontology = "FIPA-Agent-Management",
                 protocol = "fipa-request"}].

event(Self, request_event) -> {acl, request}.

action(Self, start) -> [{request_event, request_proc}].

request_proc(Self, EventName, Message, ActionName) ->
    %%io:format("Received msg ~w\n", [Message]),
    Content = Message#aclmessage.content,
    %%io:format("Received from ~w -> ~w\n",
    %%           [Message#aclmessage.sender, Content]),
    ContentReply = prepare_reply(Content),
    %%io:format("Reply ~w~n", [ContentReply]),
    acl:reply(Message, 'INFORM', ContentReply).

prepare_reply([Content = #action{'1' =
                                     #'get-description'{}}]) ->
    logger:log('AMS', "get-description"),
    APService = #'ap-service'{name =
                                  "fipa.mts.mtp.http.std",
                              type = "fipa.mts.mtp.http.std",
                              addresses = mtp:addresses()},
    APDescription = #'ap-description'{name =
                                          exat:current_platform(),
                                      'ap-services' = [APService]},
    Result = #result{'0' = Content, '1' = [APDescription]},
    [Result];
prepare_reply([Content = #action{'1' =
                                     #search{'0' =
                                                 #'ams-agent-description'{}}}]) ->
    logger:log('AMS', "search ams-agent-description"),
    Agents = [#'agent-identifier'{name = X,
                                  addresses = mtp:addresses()}
              || X <- ams:get_registered_agents()],
    Descriptions = [#'ams-agent-description'{name = X,
                                             ownership = "NONE",
                                             state = "active"}
                    || X <- Agents],
    %%io:format("DS ~w~n", [Descriptions]),
    Result = #result{'0' = Content, '1' = Descriptions},
    [Result].

%%====================================================================
%% Func: start_link/0
%% Returns: {ok, Pid}.
%%====================================================================
start_link() ->
    logger:start('AMS'),
    logger:log('AMS', "Staring AMS."),
    ontology_service:register_codec("FIPA-Agent-Management",
                                    fipa_ontology_sl_codec),
    agent:new(ams, [{behaviour, ams}]),
    [BehaviourObject] = agent:get_behaviour(ams),
    Pid = object:executorof(BehaviourObject),
    {ok, Pid}.

%%====================================================================
%% Func: register_agent/1
%% Returns: ok.
%%====================================================================
register_agent(AgentName) ->
    eresye:assert(agent_registry, {agent, AgentName}).

%%====================================================================
%% Func: de_register_agent/1
%% Returns: ok.
%%====================================================================
de_register_agent(AgentName) ->
    eresye:retract(agent_registry, {agent, AgentName}).

%%====================================================================
%% Func: get_registered_agents/0
%% Returns: [string()].
%%====================================================================
get_registered_agents() ->
    AgentList = eresye:query_kb(agent_registry,
                                {agent, '_'}),
    AgentLocalNames = [X || {_, X} <- AgentList],
    PlatformName = exat:current_platform(),
    [lists:flatten([atom_to_list(X), "@", PlatformName])
     || X <- AgentLocalNames].
