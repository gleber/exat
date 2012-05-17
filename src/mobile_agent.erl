%% Author: michal
%% Created: 17-05-2012
%% Description: TODO: Add description to mobile_agent
-module(mobile_agent).

-extends(agent).

-behaviour(agent).
-behaviour(mobile_proc).
%%
%% Include files
%%

-include("proc_mobility.hrl").

-include("agent.hrl").
%%
%% Exported Functions
%%
-export([new/3]).

%% mobile_proc callbacks
-export([init_state/1, send_me/1, register/0]).

%% gen_server callbacks
-export([handle_call/3]).

%%
%% API Functions
%%

new(AgentName, Callback, Parameters) ->
	io:format("mobile agen cretion"),
    {ok, _} = gen_server:start({local, AgentName},
                     mobile_agent, [AgentName, Callback, Parameters],
                     []),
	register().

new_with_state(AgentName, Callback, State) ->
	gen_server:start({local, AgentName},
                     mobile_agent, [AgentName, Callback, State],
                     []).

%% ====================================================================
%% Mobile Proc functions
%% ====================================================================

init_state({AgentName, Callback, State}) ->
	RunListener = fun() ->
		?INFO_MSG("Running listener ~p", self()),
		receive
			{mobility, run} ->
				Status = new_with_state(AgentName, Callback, State),
				?INFO_MSG("started with state ~p and got ~p", [State, Status]),
				proc_mobility:started(self())
		end,
		?INFO_MSG("Listener finished")
	end,
	spawn(RunListener).

send_me(Destination) ->
	gen_server:call(?MODULE, {mobility, send_me, Destination}).

register() ->
	gen_server:call(?MODULE, {mobility, register}).

%% ====================================================================
%% Gen Server 
%% ====================================================================

handle_call({mobility, send_me, Destination}, _From, State) ->
%% 	code:get_object_code(State#agent_state.callback)
	case proc_mobility:migrate(?MODULE, #mproc_state{module=?MODULE, state=State, code=[]}, Destination) of
		ok ->
			{stop, normal, ok, State};
		Result -> 
			{reply, Result, State}
	end;

handle_call({mobility, register}, _From, State) ->
	true = proc_mobility:register_name(?MODULE, self()),
	{reply, {ok, self()}, State};


handle_call(Request, From, State) ->
	agent:handle_call(Request, From, State).

%%
%% Local Functions
%%

