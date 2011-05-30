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
-include("object.hrl").
-include("acl.hrl").
-include("fipa_ontology.hrl").
-export([new/2,
         behave/2,
         get_behaviour/1,
         set_behaviour/2,
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
    gen_server:start({local, AgentName},
                     agent,
                     [AgentName, [],
                      dict:store(?AGENT_DETACHED_BEHAVIOURS, [],
                                 dict:new()),
                      []], []),
    ok.

new(AgentName, Parameters) ->
    new(AgentName),
    Rationality = prop(Parameters, rationality),
    Behaviour = prop(Parameters, behaviour),
    if
        Rationality =/= nil ->
            {Mind, Semantics} = Rationality,
            agent:set_rational(AgentName, Mind, Semantics);
        true -> nil
    end,
    if
        Behaviour =/= nil ->
            agent:set_behaviour(AgentName, Behaviour);
        true -> nil
    end,
    ok.


prop([], _) -> nil;
prop([{Key,Value}|T], Key) -> Value;
prop([_|T], Key) -> prop(T, Key).


%%
%% MAIN CALLS
%%
behave(AgentOrObject, Behaviour) ->
    Agent = extract_agent_identifier(AgentOrObject),

    CurrentBehaviour = get_behaviour(Agent),

                                                %   BehaviourObject = object:new(Behaviour),
                                                %   set_property(Agent, ?AGENT_BEHAVIOUR, BehaviourObject),
                                                %   object:bind(BehaviourObject, Agent),
                                                %   object:start(BehaviourObject),
                                                %   object:join(BehaviourObject),
                                                %   object:delete(BehaviourObject),
    set_behaviour(Agent, Behaviour),
    join(Agent),

    set_property(Agent, ?AGENT_BEHAVIOURS, CurrentBehaviour),
    ok.

get_behaviour(Agent) ->
    get_property(Agent, ?AGENT_BEHAVIOURS).


set_behaviour(Agent, Behaviours) when is_list(Behaviours) ->
    BehaviourObjectList = create_behaviours(Agent, Behaviours, []),
    set_property(Agent, ?AGENT_BEHAVIOURS, BehaviourObjectList),
    start_behaviours(BehaviourObjectList),
    ok;

set_behaviour(Agent, Behaviour) when is_atom(Behaviour) ->
    set_behaviour(Agent, [Behaviour]).


start_behaviours([]) ->
    ok;

start_behaviours([{BehaviourObject, detached} | Tail]) ->
    object:start(BehaviourObject),
    start_behaviours(Tail);

start_behaviours([BehaviourObject | Tail]) ->
    object:start(BehaviourObject),
    start_behaviours(Tail).

create_behaviours(_, [], Acc) ->
    Acc;

create_behaviours(Agent, [{BehaviourName, detached} | Tail], Acc) ->
    BehaviourObject = object:new(BehaviourName),
    object:bind(BehaviourObject, Agent),
    DetachedBehaviours = get_property(Agent, ?AGENT_DETACHED_BEHAVIOURS),
    set_property(Agent, ?AGENT_DETACHED_BEHAVIOURS,
                 [BehaviourObject | DetachedBehaviours]),
    create_behaviours(Agent, Tail, [{BehaviourObject, detached} | Acc]);

create_behaviours(Agent, [BehaviourName | Tail], Acc) ->
    BehaviourObject = object:new(BehaviourName),
    object:bind(BehaviourObject, Agent),
    create_behaviours(Agent, Tail, [BehaviourObject | Acc]).



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


extract_agent_identifier(AgentOrObject)
  when record(AgentOrObject, 'agent-identifier') ->
    AgentOrObject#'agent-identifier'.name;
extract_agent_identifier(AgentOrObject)
  when record(AgentOrObject, object) ->
    extract_agent_identifier(object:agentof(AgentOrObject));
extract_agent_identifier(AgentOrObject) ->
    AgentOrObject.



perform_message_sending(ReceiverAgent, [Mode, Message])
  when record(ReceiverAgent, 'agent-identifier') ->
    mtp:http_mtp_encode_and_send(Message);
perform_message_sending(ReceiverAgent, [Mode, Message]) ->
    gen_server:call(ReceiverAgent, [Mode, Message]).



set_rational(AgentOrObject, EngineName, SemanticsClass) ->
    SemanticsObject = object:new(SemanticsClass),
    %% io:format("Semantic Object = ~w\n", [SemanticsObject]),
    Agent = extract_agent_identifier(AgentOrObject),
    set_property(Agent, rationality, [EngineName, SemanticsObject]).

get_mind(AgentOrObject) ->
    Agent = extract_agent_identifier(AgentOrObject),
    %%io:format("Agent = ~w\n", [Agent]),
    [EngineName, SemanticsObject] = get_property(Agent, rationality),
    EngineName.

get_acl_semantics(AgentOrObject) ->
    Agent = extract_agent_identifier(AgentOrObject),
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
                                                %io:format("[Agent] Received ACL=~s\n", [Acl]),
    case catch(acl:parse_message(Acl)) of
        {'EXIT', Reason} ->
                                                %io:format("[Agent] Error in ACL parsing\n"),
            R = [AgentName, AclQueue, AgentDict, ProcessQueue];
        ParsedMessage ->
                                                %io:format("[Agent] ACL parsed OK\n"),
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


%%
%% OTHER FUNCTIONS
%%
a__set_prop(AgentDict, Property, Value) ->
    dict:store(Property, Value, AgentDict).

a__get_prop(AgentDict, Property) ->
    case dict:find(Property, AgentDict) of
        {ok, Value} -> Value;
        Other -> nil
    end.


%%
%% finds a matching message and sends it
%%
find_matching_message([], Pattern, Process, NewQueue) ->
    {false, lists:reverse(NewQueue)};

find_matching_message([Message | MessageTail], Pattern, Process, NewQueue) ->
    Match = match_lib:match_acl(Pattern, Message),
    if
        Match -> catch(Process ! Message),
                 {true, lists:reverse(NewQueue) ++ MessageTail};
        true ->
            find_matching_message(MessageTail,
                                  Pattern, Process,
                                  [ Message | NewQueue ])
    end.


%%
%% forwards a message checking if the receiving process is still alive
%%
do_forward(Message, Process) ->
    case is_process_alive(Process) of
        true -> catch(Process ! Message),
                true;
        _ -> false
    end.

%%
%% forwards a message to the process requesting a match
%%
forward_to_maching_process(Message, [], NewProcessQueue) ->
    {false, lists:reverse(NewProcessQueue)};

forward_to_maching_process(Message,
                           [{nil, Process} | ProcessQueueTail],
                           NewProcessQueue) ->
    catch(Process ! Message),
    {true, lists:reverse(NewProcessQueue) ++ ProcessQueueTail};

forward_to_maching_process(Message,
                           [{Pattern, Process} | ProcessQueueTail],
                           NewProcessQueue) ->
    Match = match_lib:match_acl(Pattern, Message),
    if
        Match -> catch(Process ! Message),
                 {true, lists:reverse(NewProcessQueue) ++ ProcessQueueTail};
        true ->
            forward_to_maching_process(Message,
                                       ProcessQueueTail,
                                       [{Pattern, Process} | NewProcessQueue])
    end.



forward_or_enqueue(AclQueue, ProcessQueue, Message) ->
    {Forwarded, NewProcessQueue} =
        forward_to_maching_process(Message, ProcessQueue, []),
    if
        Forwarded ->
            {AclQueue1, ProcessQueue1} = {AclQueue, NewProcessQueue};
        true ->
            ProcessQueue1 = ProcessQueue,
            AclQueue1 = AclQueue ++ [Message]
    end,
    {AclQueue1, ProcessQueue1}.


perform_re(Agent, AgentDict, Message, AclQueue, ProcessQueue) ->
    R = a__get_prop(AgentDict, rationality),
    CanEnqueue =
        if
            R == nil -> true;
            true ->
                [Engine, Semantics] = R,
                %% io:format("Semantics = ~w\n", [Semantics]),
                object:call(Semantics, rational_effect, [Agent, Engine, Message]),
                true
        end,
    if
        CanEnqueue ->
            forward_or_enqueue(AclQueue, ProcessQueue, Message);
        true ->
            {AclQueue, ProcessQueue}
    end.

