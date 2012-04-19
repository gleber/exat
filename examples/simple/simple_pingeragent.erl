-module(simple_pingeragent).

-behaviour(agent).

-export([start/0, stop/0]). % API

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2, terminate/2]).

-include("acl.hrl").

-include("fipa_ontology.hrl").

%%API

start() ->
    agent:new(pingeragent, ?MODULE,
              [{"localhost", 7778, "pingagent"}]).

stop() -> agent:stop(pingeragent).

%%agents callback
handle_acl(#aclmessage{speechact = 'INFORM',
                       content = "alive"} =
               Msg,
           {_, DestAgent} = State) ->
    io:format("~p is alive, since I got: ~p~n~n",
              [DestAgent, Msg]),
    {noreply, State};
handle_acl(#aclmessage{} = Msg, State) ->
    {noreply, State}.

%% gen_server callbacks

init(Name, [DestAgent]) ->
    timer:send_interval(500, ping), {ok, {Name, DestAgent}}.

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) -> {noreply, State}.

handle_info(ping, {SelfName, DestAgent} = State) ->
    {Ip, Port, Name} = DestAgent,
    Addr = lists:flatten(io_lib:format("http://~s:~b",
                                       [Ip, Port])),
    Dest = #'agent-identifier'{name = Name,
                               addresses = [Addr]},
    PingMsg = #aclmessage{sender = SelfName,
                          receiver = Dest, content = "ping"},
    spawn(fun () -> Resp = acl:query_ref(PingMsg) end),
    {noreply, State};
handle_info(Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.
