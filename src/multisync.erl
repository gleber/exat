%
% multisync.erl
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
-module (multisync).
-export ([new/0, add_task/4, wait_one/1, abort/1]).

new () ->
  O = object:new (sync),
  object:set (O, 'tasklist', []),
  O.

add_task (Sync, Module, Proc, Params) ->
  %%io:format ("Exec task ~w\n", [Proc]),
  OldList = object:get (Sync, 'tasklist'),
  Pid = spawn (Module, Proc, [Sync | Params]),
  List = [Pid | OldList],
  object:set (Sync, 'tasklist', List),
  ok.

wait_one (Sync) ->
  Result = object:call (Sync, wait),
  %%io:format ("Wait result ~w\n", [Result]),
  kill_all (object:get (Sync, 'tasklist')),
  object:delete (Sync),
  Result.

abort (Sync) ->
  kill_all (object:get (Sync, 'tasklist')),
  object:delete (Sync),
  ok.

kill_all ([]) -> nil;
kill_all ([H]) -> catch (exit (H, kill));
kill_all ([H|T]) -> kill_all ([H]), kill_all (T).
