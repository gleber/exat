%
% pingeragent.erl
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
% THIS IS THE AGENTCITIES PINGER AGENT
%-------------------------------------------
%
-module (pingeragent).
-export ([extends/0,
          pattern/2,
          event/2,
          action/2,
          on_starting/1,
          send_ping_proc/4,
          wait_pong_proc/4,
          start/0]).
-include ("acl.hrl").
-include ("fipa_ontology.hrl").

extends () -> nil.

action (Self, start) -> [{timeout_event, send_ping_proc}];
action (Self, wait_pong) -> [{acl_event, wait_pong_proc}].

event (Self, timeout_event) -> {timeout, to_value};
event (Self, acl_event) -> {acl, all_pattern}.

pattern (Self, to_value) -> 1000;
pattern (Self, all_pattern) -> [#aclmessage {}].


%%
%%
%%
on_starting (Self) ->
  logger:start ('PINGER'),
  case exat:get_argument ('destagent') of
    {ok, [PingedAgent]} ->
      {ID, ADR} = lists:splitwith (fun (X) -> X =/= $, end, PingedAgent),
      AgentID = case ADR of
                  [] -> PingedAgent;
                  [$, | ADDR] -> #'agent-identifier' { name = ID,
                                                       addresses = [ADDR] }
                end,
      object:set (Self, 'destagent', AgentID);
    _ ->
      logger:log ('PINGER', {"Invalid destination agent. Use '-destagent name,mtp_address", []}),
      object:set (Self, 'destagent', none)
  end.



%%
%% get the ping message
%%
send_ping_proc (Self, Event, _, Action) ->
  Dest = object:get (Self, 'destagent'),
  if
    Dest == none -> object:stop (Self);
    true ->
      object:set (Self, 'timestamp', erlang:now ()),
      acl:query_ref (#aclmessage { sender = Self,
                                   receiver = Dest,
                                   content = "ping" }),
      object:do (Self, wait_pong)
  end.
%%
%%
%%



%%
%% wait the 'alive' message
%%
wait_pong_proc (Self, Event, Msg = #aclmessage {speechact = 'INFORM'},
               Action) ->
  T1 = erlang:now (),
  T0 = object:get (Self, 'timestamp'),
  Diff = timer:now_diff (T1, T0) / 1000.0,
  logger:log ('PINGER', {"Received MESSAGE ~p, Elapsed ~p ms",
                         [Msg#aclmessage.content, Diff]}),
  object:do (Self, start);
%%
%%
%%
wait_pong_proc (Self, Event, Msg, Action) ->
  logger:log ('PINGER', {"BAD MESSAGE ~p", [Msg#aclmessage.content]}),
  object:do (Self, start).
%%
%%


%%
start () ->
  agent:new (pingeragent, [{behaviour, pingeragent}]).
