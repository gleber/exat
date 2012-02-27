-module(simple_pingagent).

-behaviour(simple_agent).

-export([stop/0, start/0]). % API

-export([
         init/2,
         handle_acl/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include("acl.hrl").
-include("fipa_ontology.hrl").

% API

start() ->
    simple_agent:new(pingagent, ?MODULE, []).
stop() ->
    simple_agent:stop(pingagent).


% agents callback

handle_acl(#aclmessage{speechact = 'QUERY-REF',
                       sender = Sender,
                       content = "ping"} = Msg, State) ->
    io:format("Got ping query from ~p: ~p~n~nInforming that I'm alive!~n~n", [Sender, Msg]),
    spawn(fun() -> acl:reply(Msg, 'INFORM', "alive") end),
    {noreply, State};

handle_acl(#aclmessage{} = Msg, State) ->
    {noreply, State}.



%% gen_server callbacks

init(Name, _Params) ->
    {ok, Name}.

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    {noreply, State}.

code_change(_, _, _) ->
    ok.

terminate(_, _) ->
    ok.
