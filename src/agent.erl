%%
%% agent.erl
%%
%% ----------------------------------------------------------------------
%%
%%  eXAT, an erlang eXperimental Agent Tool
%%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>

-module(agent).

-behaviour(gen_server).

-include("acl.hrl").

-include("fipa_ontology.hrl").

-export([get_acl_semantics/1, get_mind/1, get_property/2, join/1,
         kill/1, new/2, new/3, set_property/3, set_rational/3,
         stop/1, cast/2, call/2
        ]).

-export([names/1, name/1, host/1, ap/1]).

-export([code_change/3, terminate/2, handle_call/3, handle_cast/2,
         init/1, handle_info/2, behaviour_info/1]).

%% send_message/4,
%% get_message/1,
%% match_message/2,

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [{init,2},{handle_call,3},{handle_cast,2},{handle_info,2},
     {terminate,2},{code_change,3}];
behaviour_info(_Other) ->
    undefined.


-record(state,
        {name, callback, int_state, acl_queue, dict,
         process_queue}).

new(AgentName, Callback) ->
    new(AgentName, Callback, []).

new(AgentName, Callback, Parameters) ->
    gen_server:start({local, AgentName},
                     agent, [AgentName, Callback, Parameters],
                     []).

call(Ref, Call) ->
    gen_server:call(Ref, Call).
cast(Ref, Call) ->
    gen_server:cast(Ref, Call).

%%
%% MAIN CALLS
%%
join(_Agent) -> erlang:error(notimpl).

set_property(_Agent, _Property, _Value) ->
    erlang:error(notimpl).

get_property(_Agent, _Property) ->
    erlang:error(notimpl).

set_rational(_Pid, _EngineName, _SemanticsClass) ->
    erlang:error(notimpl).

get_mind(_Pid) -> erlang:error(notimpl).

get_acl_semantics(_Pid) -> erlang:error(notimpl).

stop(Agent) ->
    gen_server:cast(Agent, '$agent_stop').

kill(Agent) -> stop(Agent).

%%
%% CALLBACKS
%%

%%
%% Initialize
%%
init(Args) ->
    [AgentName, Callback, Params0 | _] = Args,
    {NoRegister, Params} = proplists_extract(no_register, Params0, false),
    case NoRegister of
        false ->
            ams:register_agent(AgentName);
        _ -> ok
    end,
    {ok, IntState} = Callback:init(AgentName, Params),
    {ok, #state{name = AgentName, callback = Callback,
                int_state = IntState}}.

%%
%% Terminate
%%
terminate(Reason,
          #state{callback = Callback, name = AgentName,
                 int_state = IntState} =
              _State) ->
    ams:de_register_agent(AgentName),
    ok = Callback:terminate(Reason, IntState),
    ok.

%%
%% Gets a property from agent
%%
handle_call({get_property, _PropertyName}, _From,
            #state{} = State) ->
    {reply, {error, notimpl}, State};
%%
%% Sets a property
%%
handle_call({set_property, _PropertyName,
             _PropertyValue},
            _From, #state{} = State) ->
    {reply, {error, notimpl}, State};
%%
%% Receives an ACL message in String format
%%
handle_call([acl, AclStr], _From,
            #state{int_state = IntState, callback = Callback} =
                State) ->
    %%io:format("[Agent] Received ACL=~s\n", [Acl]),
    case catch acl:parse_message(AclStr) of
        {'EXIT', _Reason} -> {reply, ok, State};
        Acl ->
            {noreply, IntState2} = Callback:handle_acl(Acl,
                                                       IntState),
            {reply, ok, State#state{int_state = IntState2}}
    end;
%%
%% Receives an ACL message in Erlang format
%%
handle_call([acl_erl_native, Acl], _From,
            #state{int_state = IntState, callback = Callback} =
                State) ->
    {noreply, IntState2} = Callback:handle_acl(Acl,
                                               IntState),
    {reply, ok, State#state{int_state = IntState2}};
handle_call(Call, From,
            #state{int_state = IntState, callback = Callback} =
                State) ->
    io:format("~p ~p~n", [From, Call]),
    R = Callback:handle_call(Call, From, IntState),
    IntState2 = element(size(R), R),
    setelement(size(R), R,
               State#state{int_state = IntState2}).

%%
%% Stops the agent process
%%

handle_cast('$agent_stop', State) ->
    {stop, normal, State};
handle_cast(Cast,
            #state{int_state = IntState, callback = Callback} =
                State) ->
    R = Callback:handle_cast(Cast, IntState),
    IntState2 = element(size(R), R),
    setelement(size(R), R,
               State#state{int_state = IntState2}).

handle_info(Msg,
            #state{int_state = IntState, callback = Callback} =
                State) ->
    R = Callback:handle_info(Msg, IntState),
    IntState2 = element(size(R), R),
    setelement(size(R), R,
               State#state{int_state = IntState2}).

code_change(OldVsn,
            #state{int_state = IntState, callback = Callback} =
                State,
            Extra) ->
    {ok, IntState2} = Callback:code_change(OldVsn, IntState,
                                           Extra),
    {ok, State#state{int_state = IntState2}}.

proplists_extract(Key, Proplist0, Default) ->
    Params = proplists:unfold(Proplist0),
    case lists:keytake(no_register, 1, Params) of
        {value, {Key, Val}, P} ->
            {Val, P};
        false ->
            {Default, Params}
    end.


%%
%% Utils
%%


names(List) ->
    [ name(A) || A <- List ].

name(#'agent-identifier'{name = N}) ->
    N.

ap(#'agent-identifier'{name = N}) ->
    ap(N);
ap(N) ->
    {Name, HAP} = exat:split_agent_identifier(N),
    HAP.

host(#'agent-identifier'{} = A) ->
    HAP = ap(A),
    host(HAP);
host(HAP) ->
    {_APName, Hostname} = exat:split_exat_platform_identifier(HAP),
    Hostname.
