%% Author: michal
%% Created: 17-05-2012
%% Description: TODO: Add description to mobile_agent
-module(mobile_agent).

-extends(agent).

-behaviour(agent).

%%
%% Include files
%%

-include("proc_mobility.hrl").

-include("agent.hrl").
%%
%% Exported Functions
%%
-export([new/3, new_with_state/3]).

%% gen_server callbacks
-export([handle_call/3]).

%%
%% API 
%%

new(AgentName, Callback, Parameters) ->
    io:format("mobile agent cretion ~p ~p ~p ~p", [AgentName, atom_to_list(AgentName), Callback, Parameters]),
    {match, _} = re:run(atom_to_list(AgentName), "@"),
    gen_server:start({local, AgentName},
                     mobile_agent, [AgentName, Callback, Parameters],
                     []).

new_with_state(AgentName, Callback, State) ->
	gen_server:start({local, AgentName},
                     mobile_agent, [AgentName, Callback, State],
                     []).

%% ====================================================================
%% Gen Server 
%% ====================================================================

handle_call({mobility, send_me, Destination}, _From, State) ->
%% 	code:get_object_code(State#agent_state.callback)
    case ams:get_migration_parameters(State#agent_state.name, Destination) of
        {ok, PMSAddr} ->
            State0 = {State#agent_state.name, State#agent_state.callback, State#agent_state.int_state},
            case proc_mobility:migrate(State#agent_state.name, #mproc_state{module=State#agent_state.callback, state=State0, code=[]}, PMSAddr) of
                ok ->
                    {stop, normal, ok, State};
                Result -> 
                    {reply, Result, State}
            end;
        _ ->
            {reply, {error, "Cannot migrate to given destination"}, State}
    end;


handle_call({mobility, register}, _From, State) ->
	true = proc_mobility:register_name(State#agent_state.name, self()),
	{reply, {ok, self()}, State};


handle_call(Request, From, State) ->
	agent:handle_call(Request, From, State).

%%
%% Local Functions
%%

