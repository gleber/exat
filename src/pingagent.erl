%
% pingagent.erl
%
% ----------------------------------------------------------------------
%
%  eXAT, an erlang eXperimental Agent Tool
%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%
%
%
%-------------------------------------------
% THIS IS THE AGENTCITIES PING AGENT
%-------------------------------------------
%
-module (pingagent).
-export ([extends/0,
          pattern/2,
          event/2,
          action/2,
          on_starting/1,
          get_ping_proc/4,
          start/0]).
-include ("acl.hrl").

extends () -> nil.

action (Self, start) -> [{acl_event, get_ping_proc}].

event (Self, acl_event) -> {acl, all_pattern}.

pattern (Self, all_pattern) -> [#aclmessage {}].


%%
%%
%%
on_starting (Self) ->
  logger:start ('PING-AGENT').

%%
%% get the ping message
%%
get_ping_proc (Self, Event, Msg = #aclmessage {speechact = 'QUERY-REF',
                                               content = "ping"},
               Action) ->
  logger:log ('PING-AGENT', "Received PING MESSAGE"),
  acl:reply (Msg, 'INFORM', "alive");
%%
%%
%%
get_ping_proc (Self, Event, Msg, Action) ->
  logger:log ('PING-AGENT', {"Received ~p, NOT UNDERSTOOD", [Msg]}),
  acl:reply (Msg, 'NOT-UNDERSTOOD', Msg#aclmessage.content).
%%
%%

%%
start () ->
  agent:new (pingagent, [{behaviour, pingagent}]).
