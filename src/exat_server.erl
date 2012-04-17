%%
%%exat_server.erl
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
-module(exat_server).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export([start_link/1]).

%%====================================================================
%% Internal exports
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

%%====================================================================
%% Records
%%====================================================================

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Function: start_link/1
%% Description: Starts the MTP worker
%%====================================================================

start_link(Port) ->
    io:format("~n"),
    io:format("              __      __    __    _______~n"),
    io:format("              \\ \\    / /   /  \\  "
              "|__   __|~n"),
    io:format("        ____   \\ \\  / /   / /\\ \\ "
              "   | |~n"),
    io:format("       / __ \\   \\ \\/ /   | |__| | "
              "  | |~n"),
    io:format("      | ____/   / /\\ \\   |  __  | "
              "  | |~n"),
    io:format("      | \\____  / /  \\ \\  | |  | | "
              "  | |~n"),
    io:format("       \\____/ /_/    \\_\\ |_|  |_| "
              "  |_|~n"),
    io:format("*****                               "
              "      *****************~n"),
    io:format("* The erlang eXperimental Agent Tool "
              "-- Release 1.3.0-EYE *~n"),
    io:format("*********************************************"
              "**************~n"),
    io:format("eXAT, an erlang eXperimental Agent Tool~n"),
    io:format("ERESYE, an ERlang Expert SYstem Engine~n"),
    io:format("Copyright (C) 2003-07 Corrado Santoro "
              "(csanto@diit.unict.it)~n"),
    io:format("Copyright (C) 2005-07 Francesca Gangemi "
              "(francesca@erlang-consulting.com)~n"),
    mtp:http(Port).
