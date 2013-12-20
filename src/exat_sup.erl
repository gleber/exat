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
    io:format("~n"),
    io:format("              __      __   ___    _______~n"),
    io:format("              \\ \\    / /  / _ \\  |__   __|~n"),
    io:format("        _____  \\ \\  / /  / / \\ \\    | |~n"),
    io:format("       / __  \\  \\ \\/ /   | |__| |   | |~n"),
    io:format("       | ____/  / /\\ \\   |  __  |   | |~n"),
    io:format("       | \\___  / /  \\ \\  | |  | |   | |~n"),
    io:format("       \\____/ /_/    \\_\\ |_|  |_|   |_|~n"),
    io:format("***********************************************************~n"),
    io:format("* The erlang eXperimental Agent Tool -- Release 1.3.0-EYE *~n"),
    io:format("***********************************************************~n~n"),
    io:format("eXAT, an erlang eXperimental Agent Tool~n"),
    io:format("Copyright (C)~n"),
    io:format("   2003-2007 Corrado Santoro (csanto@diit.unict.it)~n"),
    io:format("   2005-2007 Francesca Gangemi (francesca@erlang-consulting.com)~n"),
    io:format("   2010-2013 Gleb Peregud (gleber.p@gmail.com)~n"),

    Port = case init:get_argument(http_port) of
               {ok, [[ThePort]]} -> list_to_integer(ThePort);
               _ -> (?DEFAULT_PORT)
           end,
    MTP =
        {exat_mtp_server,
         {exat_mtp_server, start_link, [Port]},
         permanent, 10000, worker, [exat_mtp_server]},
    AMS =
        {exat_ams, {ams, start_link, []}, 
         permanent, 10000, worker, [ams]},
    ONTO =
        {ontology_service,
         {gen_server, start_link, [{local, ontology_service}, ontology_service, [], []]},
         permanent, 10000, worker, [ontology_service]},
    ExatChildSpec = [
                     ONTO,
                     AMS,
                     MTP
                    ],
    {ok, {{one_for_all, 5, 20}, ExatChildSpec}}.
