%%
%% exat.erl
%%
%% ----------------------------------------------------------------------
%%
%%  eXAT, an erlang eXperimental Agent Tool
%%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%%
-module(exat).

-behaviour(application).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export([current_platform/0, get_argument/1,
         split_agent_identifier/1, split_exat_platform_identifier/1,

         start/0, stop/0,

         start/2, stop/1]).

%%====================================================================
%% Internal exports
%%====================================================================

-export([]).

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
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%====================================================================

start() ->
    ok = application:ensure_started(inets, permanent),  % FIXME! Use ".rel"
    ok = application:ensure_started(xmerl, permanent),
    ok = application:ensure_started(ranch, permanent),
    ok = application:ensure_started(cowlib, permanent),
    ok = application:ensure_started(cowboy, permanent),
    ok = application:ensure_started(erlware_commons),
    ok = application:ensure_started(seresye),
    ok = application:ensure_started(exat, permanent),
    case init:get_argument(start) of
        {ok, [[List]]} ->
            ApplicationList = string:tokens(List, ","),
            case ApplicationList of
                [] -> ok;
                _ ->
                    logger:log('AMS',
                               {"Staring Applications: ~s",
                                [lists:flatten(["{" ++ X ++ "}"
                                                || X <- ApplicationList])]}),
                    lists:foreach(fun (X) ->
                                          M = list_to_atom(X),
                                          io:format("~p~n", [M]),
                                          M:start()
                                  end,
                                  ApplicationList)
            end;
        _ -> ok
    end.

%%====================================================================
%% Func: stop/1
%% Returns: any
%%====================================================================

stop() ->
    ok = application:stop(exat).


start(_Type, _StartArgs) ->
    exat_sup:start_link().

%%====================================================================
%% Func: stop/1
%% Returns: any
%%====================================================================
stop(_State) -> 
    ok.

%%====================================================================
%% Func: current_platform/0
%% Returns: string()
%%====================================================================
current_platform() ->
    [Node, Host] = string:tokens(atom_to_list(node()), "@"),
    iolist_to_binary([Node, ":", Host]).

%%====================================================================
%% Func: split_agent_identifier/1
%% Returns: {string(), string()}
%%====================================================================
split_agent_identifier(AgentID) ->
    case binary:split(AgentID, <<"@">>) of
        [LocalID] -> {LocalID, current_platform()};
        [LocalID, RealHAP] -> {LocalID, RealHAP}
    end.

split_exat_platform_identifier(AP) ->
    [APName, APHost] = binary:split(AP, <<":">>),
    {APName, APHost}.


%%====================================================================
%% Func: get_argument/1
%% Returns: {ok, [string()]} | error
%%====================================================================
get_argument(Name) ->
    case init:get_argument(Name) of
        {ok, [List]} -> {ok, List};
        _ -> error
    end.

%%====================================================================
%% Internal functions
%%====================================================================
