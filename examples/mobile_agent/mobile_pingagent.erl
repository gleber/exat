-module(mobile_pingagent).

-behaviour(mobile_agent).
-behaviour(mobile_proc).

-export([start/0, stop/0]). % API

-export([code_change/3, handle_acl/2, handle_call/3,
         handle_cast/2, handle_info/2, init/2, terminate/2]).

%% mobile_proc callbacks
-export([init_state/1, send_me/1, register/0]).

-include("acl.hrl").

-include("fipa_ontology.hrl").

%%API

start() -> 
	application:start(gproc),
	application:start(proc_mobility),
	mobile_agent:new(pingagent, ?MODULE, []),
	register().

stop() -> mobile_agent:stop(pingagent).

%%agents callback

handle_acl(#aclmessage{speechact = 'QUERY-REF',
                       sender = Sender, content = <<"ping">>} =
               Msg,
           State) ->
    io:format("Got ping query from ~p: ~p~n~nInforming "
              "that I'm alive!~n~n",
              [Sender, Msg]),
    spawn(fun () -> acl:reply(Msg, 'INFORM', <<"alive">>) end),
    {noreply, State};
handle_acl(#aclmessage{} = Msg, State) ->
	io:format("unknown ~p~n", [Msg]),
    {noreply, State}.

%% ====================================================================
%% Mobile Proc functions
%% ====================================================================

init_state({AgentName, Callback, State}) ->
	RunListener = fun() ->
		io:format("Running listener ~p", [self()]),
		receive
			{mobility, run} ->
				Status = mobile_agent:new_with_state(AgentName, Callback, [State]),
				io:format("started with state ~p and got ~p", [State, Status]),
				proc_mobility:started(self())
		end,
		io:format("Listener finished")
	end,
	spawn(RunListener).

send_me(Destination) ->
	gen_server:call(pingagent, {mobility, send_me, Destination}).

register() ->
	gen_server:call(pingagent, {mobility, register}).

%% gen_server callbacks

init(Name, _Params) -> {ok, Name}.

handle_call(Call, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Call, State) -> {noreply, State}.

handle_info(Msg, State) -> {noreply, State}.

code_change(_, State, _) -> {ok, State}.

terminate(_, _) -> ok.
