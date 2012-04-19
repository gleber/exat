-module(exat_agent).

-export([start/0]).

-behaviour(agent).

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2,
         terminate/2]).

-include_lib("exat/include/acl.hrl").

-include_lib("exat/include/fipa_ontology.hrl").

-record(state, {name}).

start() ->
    agent:new(the_exat_agent, the_exat_agent, [{behaviour, exat_agent}]).

handle_acl(#aclmessage{speechact = 'REQUEST'} = Message, #state{self = Self} = State) ->
    io:format("[Agent:~w] Request received from agent ~p\n",
              [Self, Message#aclmessage.sender]),
    
    {noreply, State}.

init(the_exat_agent, Params) ->
    acl:sendacl(#aclmessage{speechact = 'REQUEST',
                            content = "ping", sender = Self,
                            receiver = fellow_agent()}),
    {ok, #state{self = the_exat_agent}}.

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) ->
    {reply, {error, unknown_cast}, State}.

handle_info(Msg, State) ->
    {noreply, State}.

code_change(OldVsn, State, Extra) -> {ok, State}.

terminate(_Reason, _State) ->
    ok.


fellow_agent() ->
    #'agent-identifier'{name = "jadeagent@jadeplatform",
                        addresses = ["http://localhost:7778/acc"]}.
