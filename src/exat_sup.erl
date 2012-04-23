%%
%%exat_sup.erl
%%
%%----------------------------------------------------------------------
%%
%% eXAT, an erlang eXperimental Agent Tool
%% Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%%
-module(exat_sup).

-behaviour(supervisor).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export([start_link/0]).

%%====================================================================
%% Internal exports
%%====================================================================

-export([init/1]).

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_PORT, 7779).

%%====================================================================
%% Records
%%====================================================================

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Function: start_link/0
%% Description: Starts the supervisor
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Internal functions
%%====================================================================
%%====================================================================
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%====================================================================

init([]) ->
    case init:get_argument(http_port) of
        {ok, [[ThePort]]} -> Port = list_to_integer(ThePort);
        _ -> Port = (?DEFAULT_PORT)
    end,
    {ok, _Pid} = seresye:start(agent_registry),
    MTP = {exat_platform, {exat_server, start_link, [Port]},
           permanent, brutal_kill, supervisor, [exat_server]},
    MTP_SENDER = {mtp_sender_service,
                  {gen_server, start_link,
                   [{local, mtp_sender}, mtp, [], []]},
                  permanent, brutal_kill, worker, [ontology_service]},
    AMS = {exat_ams, {ams, start_link, []}, permanent,
           brutal_kill, worker, [ams]},
    ONTO = {ontology_service,
            {gen_server, start_link,
             [{local, ontology_service}, ontology_service, [], []]},
            permanent, brutal_kill, worker, [ontology_service]},
    ExatChildSpec = [MTP, 
                     MTP_SENDER, 
                     ONTO, 
                     AMS
                    ],
    {ok, {{one_for_all, 5, 20}, ExatChildSpec}}.
