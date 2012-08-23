-module(mobile_pingagent).

-behaviour(mobile_agent).
-behaviour(mobile_proc).

-export([start/0, stop/0]). % API

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2, terminate/2, ams_register_acl/0]).

%% mobile_proc callbacks
-export([init_state/1, send_me/1, register/0]).

-include("acl.hrl").

-include("fipa_ontology.hrl").

%%API

start() -> 
	application:start(gproc),
	application:start(proc_mobility),
    mobile_agent:new(agent:full_local_name("pingagent"), ?MODULE, []),
	register().

stop() -> mobile_agent:stop(pingagent).

%%agents callback

handle_acl(#aclmessage{speechact = 'QUERY-REF',
                       sender = Sender, content = <<"ping">>} =
               Msg,
           {Name, Acc}) ->
    io:format("Got ping query ~p from ~p: ~p~n~nInforming "
              "that I'm alive!~n~n",
              [Acc, Sender, Msg]),
    spawn(fun () -> acl:reply(Msg, 'INFORM', <<"alive">>) end),
    {noreply, {Name, Acc+1}};
handle_acl(#aclmessage{} = Msg, State) ->
    io:format("unknown ~p~n", [Msg]),
    {noreply, State}.

%% ====================================================================
%% Mobile Proc functions
%% ====================================================================

init_state({AgentName, Callback, State}) ->
    Status = mobile_agent:new_with_state(AgentName, Callback, [{saved_state, State}]),
    io:format("started with state ~p and got ~p", [State, Status]).

send_me(Destination) ->
    gen_server:call(mobile_agent:full_local_name("pingagent"), {mobility, send_me, Destination}).

register() ->
    gen_server:call(mobile_agent:full_local_name("pingagent"), {mobility, register}).

%% gen_server callbacks

init(Name, [{saved_state, State}]) ->
    proc_mobility:register_name(Name, self()),
    {ok, State};

init(Name, _Params) ->
    {ok, {Name, 0}}.


handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) -> {noreply, State}.

handle_info(Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.

ams_register_acl() ->
    Addr = <<"http://localhost:7778">>,
    Dest = #'agent-identifier'{name = <<"ams">>, addresses=[Addr]},
    Content = #action{
            '0' = Dest,
            '1' = #'register'{
                name=#'agent-identifier'{name = <<"pingagent2">>, 
                                         addresses=[<<"http://127.0.0.1:7778">>]}}},
    PingMsg = #aclmessage{sender = pingagent,
                          receiver = ams,
                          content = Content,
                          ontology = ?FIPA_AGENT_MANAGEMENT,
                          protocol = <<"fipa-request">>
                         },
    io:format("acl request ~p~n", [acl:request(PingMsg)]).

