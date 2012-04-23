%%
%%fipa_semantics_simple.erl
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
-module(fipa_semantics_simple).

-export([extends/0, is_feasible/4, rational_effect/4]).

-include("acl.hrl").

extends() -> semantics.

is_feasible(_Self, _Agent, _KB, #aclmessage{speechact = 'INFORM'}) ->
    %%io:format ("[Agent: ~w] Doing FP test on ~w\n", [Agent, AclMessage]),
    true.

rational_effect(_Self, _Agent, KB, AclMessage = #aclmessage{speechact = 'INFORM'}) ->
    %%io:format ("[Agent: ~w] Performing RE on ~p\n", [Agent, AclMessage]),
    seresye:assert(KB, AclMessage#aclmessage.content),
    true.
