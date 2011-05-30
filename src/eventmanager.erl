%%
%% eventmanager.erl
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
-module(eventmanager).
-export([silent_event/4,
         timeout_event/4,
         acl_event/4,
         eres_event/4,
         eventof/1]).

eventof(timeout) -> timeout_event;
eventof(acl) -> acl_event;
eventof(eres) -> eres_event;
eventof(eresye) -> eres_event;
eventof(silent) -> silent_event.

silent_event(Sync, Agent, Pattern, Proc) ->
                                                %io:format("Silent\n"),
    object:call(Sync, signal, [{silent, Pattern, Proc}]).

timeout_event(Sync, Agent, Timeout, Proc) ->
                                                %io:format("Timeout ~w\n", [Timeout]),
    timer:sleep(Timeout),
    object:call(Sync, signal, [{timeout, Timeout, Proc}]).

acl_event(Sync, Agent, Pattern, Proc) ->
                                                %   Message = agent:get_message(Agent),
                                                %   Match = match_lib:match_acl(Pattern, Message),
                                                %   %%io:format("ACL Event: ~w\n", [Message]),
                                                %   %%io:format("ACL Match: ~w\n", [Match]),
                                                %   case Match of
                                                %     false -> acl_event(Sync, Agent, Pattern, Proc);
                                                %     true -> object:call(Sync, signal, [{acl, Message, Proc}])
                                                %   end.
    Message = agent:match_message(Agent, Pattern),
    object:call(Sync, signal, [{acl, Message, Proc}]).

eres_event(Sync, Agent, {Engine, read, Pattern}, Proc) ->
    Data = eresye:wait(Engine, Pattern),
    object:call(Sync, signal, [{eres, Data, Proc}]);

eres_event(Sync, Agent, {Engine, get, Pattern}, Proc) ->
    Data = eresye:wait_and_retract(Engine, Pattern),
                                                %io:format("eres got ~w\n", [Data]),
    object:call(Sync, signal, [{eres, Data, Proc}]).
