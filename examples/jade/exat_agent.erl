-module(exat_agent).
-export([extends/0]).
-export([pattern/2,event/2,action/2,on_starting/1,
  do_request/4,start/0]).

-include_lib("exat/include/acl.hrl").
-include_lib("exat/include/fipa_ontology.hrl").

extends()-> nil.

pattern(Self, request)-> [#aclmessage{speechact='REQUEST'}].

event(Self, evt_request)-> {acl, request}.

action(Self, start)-> {evt_request, do_request}.

fellow_agent()-> #'agent-identifier'{
  name = "jadeagent@jadeplatform",
  addresses = ["http://localhost:7778/acc"]}.

on_starting(Self)->
  io:format("[Agent:~w] Starting\n", [object:agentof(Self)]),
  acl:sendacl(#aclmessage{speechact = 'REQUEST',
    content = "ping", sender = Self,
    receiver = fellow_agent()}).

do_request(Self, EventName, Message, ActionName)->
  io:format("[Agent:~w] Request received from agent ~p\n",
    [object:agentof(Self), Message#aclmessage.sender]),
  object:do(Self, start).

start()->
  agent:new(the_exat_agent,[{behaviour, exat_agent}]).

