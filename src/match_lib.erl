%%
%%match_lib.erl
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
-module(match_lib).

-export([match_acl/2, match_all/2]).

-author('csanto@diit.unict.it').

-include("acl.hrl").

match_atoms(F, Atom) when is_function(F) -> F(Atom);
match_atoms(Atom1, Atom2) ->
    (Atom1 == (?ACL_ANY)) or (Atom2 == (?ACL_ANY)) or
                                                     (Atom1 == Atom2).

%%matches lists
match_lists([], []) -> true;
match_lists([L1], [L2]) -> match_atoms(L1, L2);
match_lists([_L], [_H | _T]) -> false;
match_lists([_H | _T], [_L]) -> false;
match_lists([H1 | T1], [H2 | T2]) ->
    X = match_atoms(H1, H2),
    if X -> match_lists(T1, T2);
       true -> false
    end.

%%matches lists or tuples
match_all(Fact1, Fact2) ->
    Tuple1 = is_tuple(Fact1),
    Tuple2 = is_tuple(Fact2),
    if Tuple1 and Tuple2 ->
            match_lists(tuple_to_list(Fact1), tuple_to_list(Fact2));
       not Tuple1 and Tuple2 -> false;
       Tuple1 and not Tuple2 -> false;
       true -> match_lists(Fact1, Fact2)
    end.

%%
%%example:
%% match_lib:match_acl ({aclmessage,infom,nil,..,..,..,..}).
%%
match_acl([], _Data) -> false;
match_acl([Clause | Clauses], Data) ->
    Match = match_all(Clause, Data),
    if Match -> true;
       true -> match_acl(Clauses, Data)
    end.

%%build_fun ([]) -> "".
