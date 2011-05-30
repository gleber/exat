%%
%% simple_agent.erl
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


-module(simple_agent).
-behaviour(gen_server).
-include("acl.hrl").
-include("fipa_ontology.hrl").
-export([new/2,
         join/1,
         stop/1,
         kill/1,
         set_rational/3,
         get_mind/1,
         get_acl_semantics/1,
         set_property/3,
         get_property/2,
         send_message/4,
         get_message/1,
         match_message/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

-define(AGENT_BEHAVIOURS, '__AGENT_BEHAVIOURS').
-define(AGENT_DETACHED_BEHAVIOURS, '__AGENT_DETACHED_BEHAVIOURS').

new(AgentName) ->
    new(AgentName, []).

new(AgentName, Parameters) ->
    {ok, _} = gen_server:start({local, AgentName},
                               simple_agent,
                               [AgentName, Parameters], []),
    ok.


%%
%% MAIN CALLS
%%



join(Agent) ->
    BehaviourObject = get_property(Agent, ?AGENT_BEHAVIOURS),
    join_list(BehaviourObject),
    set_property(Agent, ?AGENT_BEHAVIOURS, nil),
    ok.

join_list(nil) -> ok;
join_list([]) -> ok;
join_list([{BehaviourObject, detached} | Tail]) -> join_list(Tail);
join_list([BehaviourObject | Tail]) ->
    object:join(BehaviourObject),
    object:delete(BehaviourObject),
    join_list(Tail).

set_property(Agent, Property, Value) ->
    gen_server:call(Agent, [set_property, Property, Value]).

get_property(Agent, Property) ->
    gen_server:call(Agent, [get_property, Property]).

get_message(Agent) ->
    gen_server:cast(Agent, [getmessage, self()]),
    receive
        X -> X
    end.

match_message(Agent, Pattern) ->
    gen_server:cast(Agent, [match_message, Pattern, self()]),
    receive
        X -> X
    end.

send_message(nil, ReceiverAgent, Mode, Message) ->
    gen_server:call(ReceiverAgent, [Mode, Message]), ok;

send_message(SenderAgent, ReceiverAgent, Mode, Message) ->
    SenderAgentName = extract_agent_identifier(SenderAgent),
                                                %io:format("name = ~w\n", [SenderAgentName]),
    R = agent:get_property(SenderAgentName, rationality),
    CanSend =
        if
            R == nil -> true;
            true ->
                [Engine, Semantics] = R,
                %%io:format("Semantics = ~w,~w,~w\n",
                %%           [Engine, Semantics, SenderAgentName]),
                object:call(Semantics, is_feasible,
                            [SenderAgentName, Engine, Message])
        end,
                                                %io:format("Cansend = ~w\n", [CanSend]),
    if
        CanSend -> perform_message_sending(ReceiverAgent, [Mode, Message]), ok;
%%% FIXME!!!! This is not for a distributed environment?????
        true -> error
    end.


extract_agent_identifier(Pid)
  when record(Pid, 'agent-identifier') ->
    Pid#'agent-identifier'.name;
extract_agent_identifier(Pid)
  when record(Pid, object) ->
    extract_agent_identifier(object:agentof(Pid));
extract_agent_identifier(Pid) ->
    Pid.



perform_message_sending(ReceiverAgent, [Mode, Message])
  when record(ReceiverAgent, 'agent-identifier') ->
    mtp:http_mtp_encode_and_send(Message);
perform_message_sending(ReceiverAgent, [Mode, Message]) ->
    gen_server:call(ReceiverAgent, [Mode, Message]).



set_rational(Pid, EngineName, SemanticsClass) ->
    SemanticsObject = object:new(SemanticsClass),
    %% io:format("Semantic Object = ~w\n", [SemanticsObject]),
    Agent = extract_agent_identifier(Pid),
    set_property(Agent, rationality, [EngineName, SemanticsObject]).

get_mind(Pid) ->
    Agent = extract_agent_identifier(Pid),
    %%io:format("Agent = ~w\n", [Agent]),
    [EngineName, SemanticsObject] = get_property(Agent, rationality),
    EngineName.

get_acl_semantics(Pid) ->
    Agent = extract_agent_identifier(Pid),
    Semantics = get_property(Agent, rationality),
    if
        Semantics == nil -> nil;
        true ->
            [EngineName, SemanticsObject] = get_property(Agent, rationality),
            SemanticsObject
    end.


stop(Agent) ->
    agent:join(Agent),
    Semantics = get_acl_semantics(Agent),
    if
        Semantics =/= nil -> object:delete(Semantics);
        true -> nil
    end,
    gen_server:cast(Agent, stop),
    ok.

kill(Agent) ->
    BehaviourObject = get_property(Agent, ?AGENT_BEHAVIOURS),
    kill_list(BehaviourObject),
    DetachedBehaviourObject = get_property(Agent, ?AGENT_DETACHED_BEHAVIOURS),
    kill_list(DetachedBehaviourObject),
    set_property(Agent, ?AGENT_BEHAVIOURS, nil),
    Semantics = get_acl_semantics(Agent),
    if
        Semantics =/= nil ->
            object:delete(Semantics);
        true -> nil
    end,
    gen_server:cast(Agent, stop),
    ok.

kill_list(nil) -> ok;
kill_list([]) -> ok;
kill_list([{BehaviourObject, detached} | Tail]) ->
    object:delete(BehaviourObject),
    kill_list(Tail);
kill_list([BehaviourObject | Tail]) ->
    object:delete(BehaviourObject),
    kill_list(Tail).

%%
%% CALLBACKS
%%

%%
%% Initialize
%%
init(State) ->
    [ AgentName | _ ] = State,
    ams:register_agent(AgentName),
    {ok, State}.

%%
%% Terminate
%%
terminate(normal, State) ->
    [ AgentName | _ ] = State,
    ams:de_register_agent(AgentName),
    ok.

%%
%% Gets a property from agent
%%
handle_call([get_property, PropertyName], From,
            [AgentName, AclQueue, AgentDict, ProcessQueue]) ->
    Value = a__get_prop(AgentDict, PropertyName),
    {reply, Value, [AgentName, AclQueue, AgentDict, ProcessQueue]};


%%
%% Sets a property
%%
handle_call([set_property, PropertyName, PropertyValue], From,
            [AgentName, AclQueue, AgentDict, ProcessQueue]) ->
    A1 = a__set_prop(AgentDict, PropertyName, PropertyValue),
    {reply, ok, [AgentName, AclQueue, A1, ProcessQueue]};



%%
%% Receives an ACL message in String format
%%
handle_call([acl, Acl], From,
            [AgentName, AclQueue, AgentDict, ProcessQueue]) ->
    %%io:format("[Agent] Received ACL=~s\n", [Acl]),
    case catch(acl:parse_message(Acl)) of
        {'EXIT', Reason} ->
            %%io:format("[Agent] Error in ACL parsing\n"),
            R = [AgentName, AclQueue, AgentDict, ProcessQueue];
        ParsedMessage ->
            %%io:format("[Agent] ACL parsed OK\n"),
            {AclQueue1, ProcessQueue1} =
                perform_re(AgentName, AgentDict,
                           ParsedMessage, AclQueue, ProcessQueue),
            R = [AgentName, AclQueue1, AgentDict, ProcessQueue1]
    end,
    {reply, ok, R};

%%
%% Receives an ACL message in Erlang format
%%
handle_call([acl_erl_native, Acl], From,
            [AgentName, AclQueue, AgentDict, ProcessQueue]) ->
    %%io:format("[Agent] Received ACL=~w\n", [Acl]),
    {AclQueue1, ProcessQueue1} =
        perform_re(AgentName, AgentDict,
                   Acl, AclQueue, ProcessQueue),
    {reply, ok, [AgentName, AclQueue1, AgentDict, ProcessQueue1]}.


%%
%% Retrieves a message from the queue(or waits for the message)
%%
handle_cast([getmessage, From],
            [AgentName, AclQueue, AgentDict, ProcessQueue]) ->
    if
        length(AclQueue) > 0 ->
            ProcessQueue1  = ProcessQueue,
            [Message | AclQueue1] = AclQueue,
            catch(From ! Message);
        true ->
            ProcessQueue1 = ProcessQueue ++ [{nil, From}],
            AclQueue1 = AclQueue
    end,
    {noreply, [AgentName, AclQueue1, AgentDict, ProcessQueue1]};

%%
%% Retrieves a matching message from the queue
%% (or waits for a matching message)
%%
handle_cast([match_message, Pattern, From],
            [AgentName, AclQueue, AgentDict, ProcessQueue]) ->
    {Sent, NewAclQueue} = find_matching_message(AclQueue, Pattern, From, []),
    if
        Sent ->
            AclQueue1 = NewAclQueue,
            ProcessQueue1 = ProcessQueue;
        true ->
            ProcessQueue1 = ProcessQueue ++ [{Pattern, From}],
            AclQueue1 = AclQueue
    end,
                                                %   if
                                                %     length(AclQueue) > 0 ->
                                                %       ProcessQueue1  = ProcessQueue,
                                                %       [Message | AclQueue1] = AclQueue,
                                                %       catch(From ! Message);
                                                %     true ->
                                                %       ProcessQueue1 = ProcessQueue ++ [{Pattern, From}],
                                                %       AclQueue1 = AclQueue
                                                %   end,
    {noreply, [AgentName, AclQueue1, AgentDict, ProcessQueue1]};


%%
%% Stops the agent process
%%
handle_cast(stop, State) ->
    {stop, normal, State}.

