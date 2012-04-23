%%
%% aclscan.erl
%% - New acl scanner
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
-module(aclscan).

-export([scan/1]).

scan([], _Acc, Tokens) -> lists:reverse(Tokens);
scan([$( | L], [], Tokens) ->
    scan(L, [], ["(" | Tokens]);
scan([$( | L], Acc, Tokens) ->
    scan(L, [], ["(", lists:reverse(Acc) | Tokens]);
scan([$) | L], [], Tokens) ->
    scan(L, [], [")" | Tokens]);
scan([$) | L], Acc, Tokens) ->
    scan(L, [], [")", lists:reverse(Acc) | Tokens]);
scan([$: | L], [], Tokens) ->
    scan(L, [], [":" | Tokens]);
scan([$: | L], Acc, Tokens) ->
    scan(L, [], [":", lists:reverse(Acc) | Tokens]);
scan([$" | L], Acc, Tokens) ->
    [Data, Tail] = toquote(L),
    scan(Tail, [], [Data, lists:reverse(Acc) | Tokens]);
scan([$\s | L], Acc, Tokens) ->
    scan(L, [], [lists:reverse(Acc) | Tokens]);
scan([$\\, H | L], Acc, Tokens) ->
    scan(L, [H | Acc], Tokens);
scan([H | L], Acc, Tokens) ->
    scan(L, [H | Acc], Tokens).

toquote([]) -> [[], []];
toquote([$" | T]) -> [[], T];
toquote([$\\, H | T]) ->
    [Data, Tail] = toquote(T), [[H | Data], Tail];
toquote([H | T]) ->
    [Data, Tail] = toquote(T), [[H | Data], Tail].

remove_empty([]) -> [];
remove_empty([[] | T]) -> remove_empty(T);
remove_empty([H | T]) -> [H | remove_empty(T)].

scan(Message) -> remove_empty(scan(Message, [], [])).
