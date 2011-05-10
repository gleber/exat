%
% semantics.erl
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
-module (semantics).
-export ([extends/0, semantics/1, semantics_/1,
         is_feasible/4, rational_effect/4]).
-include ("acl.hrl").

extends () -> nil.

semantics (Self) -> nil.

semantics_ (Self) -> nil.

is_feasible (Self, Agent, KB, AclMessage) -> true.

rational_effect (Self, Agent, KB, AclMessage) -> true.

