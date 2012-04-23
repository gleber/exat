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

-export([de_register_agent/1, get_registered_agents/0,
         register_agent/1, start_link/0]).

-behaviour(agent).

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2,
         terminate/2]).

-include_lib("exat/include/acl.hrl").

-include_lib("exat/include/fipa_ontology.hrl").

-record(state, {}).

handle_acl(#aclmessage{speechact = 'REQUEST',
                       ontology = "FIPA-Agent-Management",
                       protocol = "fipa-request"} = Message,
           State) ->
    lists:foreach(fun(Content) ->
                          ContentReply = prepare_reply(Content),
                          spawn(fun () -> acl:reply(Message, 'INFORM', ContentReply) end)
                  end, Message#aclmessage.content),

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
    Agents = [#'agent-identifier'{name = X,
                                  addresses = mtp:addresses()}
              || X <- ams:get_registered_agents()],
    Descriptions = [#'ams-agent-description'{name = X,
                                             ownership = "NONE",
                                             state = "active"}
                    || X <- Agents],
    #result{'0' = Content, '1' = Descriptions}.

%%====================================================================
%% Func: start_link/0
%% Returns: {ok, Pid}.
%%====================================================================
start_link() ->
    agent:new(ams, ams, [{behaviour, ams}, no_register]).

init(ams, _Params) ->
    logger:start('AMS'),
    logger:log('AMS', "Staring AMS."),
    ontology_service:register_codec("FIPA-Agent-Management",
                                    fipa_ontology_sl_codec),
    {ok, #state{}}.


handle_call({register, Agent, Pid}, _From, State) ->
    Ref = erlang:monitor(process, Pid),
    seresye:assert(agent_registry, {agent, Agent, Ref}),
    {reply, ok, State};
    
handle_call(Call, _From, State) ->
    {reply, {error, unknown_call, Call}, State}.

handle_cast(Cast, State) ->
    {reply, {error, unknown_cast, Cast}, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, State) ->
    seresye:retract_match(agent_registry, {agent, '_', Ref}),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Func: register_agent/1
%% Returns: ok.
%%====================================================================
register_agent(AgentName) ->
    register_agent(AgentName, self()).

register_agent(AgentName, Pid) ->
    agent:call(ams, {register, AgentName, Pid}).

%%====================================================================
%% Func: de_register_agent/1
%% Returns: ok.
%%====================================================================
de_register_agent(AgentName) ->
    seresye:retract_match(agent_registry, {agent, AgentName, '_'}).

%%====================================================================
%% Func: get_registered_agents/0
%% Returns: [string()].
%%====================================================================
get_registered_agents() ->
    AgentList = seresye:query_kb(agent_registry,
                                 {agent, '_', '_'}),
    AgentLocalNames = [X || {_, X} <- AgentList],
    PlatformName = exat:current_platform(),
    [lists:flatten([atom_to_list(X), "@", PlatformName])
     || X <- AgentLocalNames].
