-module(noop_agent).

-behaviour(agent).

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2, terminate/2]).

-include("acl.hrl").

-include("fipa_ontology.hrl").

%%API
 
%%agents callback
handle_acl(#aclmessage{} = _Msg, State) ->
    {noreply, State}.

%% gen_server callbacks

init(_Name, Params) ->
    State = proplists:get_value(state, Params, undefined),
    {ok, State}.

handle_call(_Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) -> 
    {noreply, State}.

handle_info(_Msg, State) -> 
    {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.
