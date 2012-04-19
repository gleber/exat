-module(simple_pingagent).

-behaviour(agent).

-export([start/0, stop/0]). % API

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2, terminate/2]).

-include("acl.hrl").

-include("fipa_ontology.hrl").

%%API

start() -> agent:new(pingagent, ?MODULE, []).

stop() -> agent:stop(pingagent).

%%agents callback

handle_acl(#aclmessage{speechact = 'QUERY-REF',
                       sender = Sender, content = "ping"} =
               Msg,
           State) ->
    io:format("Got ping query from ~p: ~p~n~nInforming "
              "that I'm alive!~n~n",
              [Sender, Msg]),
    spawn(fun () -> acl:reply(Msg, 'INFORM', "alive") end),
    {noreply, State};
handle_acl(#aclmessage{} = Msg, State) ->
    {noreply, State}.

%% gen_server callbacks

init(Name, _Params) -> {ok, Name}.

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) -> {noreply, State}.

handle_info(Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.
