-module(simple_pingeragent).

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
    simple_agent:new(pingeragent, ?MODULE, [{"localhost", 7778, "pingagent"}]).
stop() ->
    simple_agent:stop(pingeragent).


% agents callback
handle_acl(#aclmessage{speechact = 'INFORM',
                       content = "alive"} = Msg, {_, DestAgent} = State) ->
    io:format("~p is alive!", [DestAgent]),
    {noreply, State};

handle_acl(#aclmessage{} = Msg, State) ->
    {noreply, State}.



%% gen_server callbacks

init(Name, DestAgent) ->
    timer:send_interval(5000, ping),
    {ok, {Name, DestAgent}}.

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) ->
    {noreply, State}.


handle_info(ping, {Name, DestAgent} = State) ->
    {Ip, Port, Name} = DestAgent,
    Addr = lists:flatten(io_lib:format("http://~s:~b", [Ip, Port])),
    Dest = #'agent-identifier'{name = binary_to_list(Name),
                               addresses = [Addr]},
    PingMsg = #aclmessage{sender = Name,
                          receiver = Dest,
                          content = "ping"},
    Resp = acl:query_ref(PingMsg),
    {noreply, State};

handle_info(Msg, State) ->
    {noreply, State}.

code_change(_, _, _) ->
    ok.

terminate(_, _) ->
    ok.
