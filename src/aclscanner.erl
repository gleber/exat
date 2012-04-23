%%
%%aclscanner
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
-module(aclscanner).

-export([scan/1, skiptoquote/2, token/1, tokenize/1]).

accumulate(L) ->
    [Tokens, Acc] = L,
    if length(Acc) == 0 -> [Tokens, []];
       true -> [Tokens ++ [Acc], []]
    end.

skiptoquote([], Acc) -> [Acc, ""];
skiptoquote([$"], Acc) -> [Acc, ""];
skiptoquote([$" | T], Acc) -> [Acc, T];
skiptoquote([H | T], Acc) -> skiptoquote(T, Acc ++ [H]).

scan([], L) -> accumulate(L);
scan([32], L) -> accumulate(L);
scan([$(], L) ->
    [Tokens, _Acc] = accumulate(L), [Tokens ++ ["("], []];
scan([$)], L) ->
    [Tokens, _Acc] = accumulate(L), [Tokens ++ [")"], []];
scan([L], L2) ->
    [Tokens, Acc] = L2, [Tokens ++ [Acc ++ [L]], []];
scan([$( | T], L) ->
    [Tokens, _Acc] = accumulate(L),
    scan(T, [Tokens ++ ["("], []]);
scan([$) | T], L) ->
    [Tokens, _Acc] = accumulate(L),
    scan(T, [Tokens ++ [")"], []]);
scan([$\s, $: | T], L) ->
    [Tokens, _Acc] = accumulate(L),
    scan(T, [Tokens ++ [":"], []]);
%%scan ([$ |T], L) ->
%% [Tokens, Acc] = accumulate (L),
%% scan (T, [Tokens ++ [" "], []]);
scan([H | T], L) ->
    [Tokens, Acc] = L,
    case H of
        32 -> L2 = accumulate(L), T2 = T;
        34 ->
            [Tokens2, _Acc2] = accumulate(L),
            [H2, T2] = skiptoquote(T, []),
            L2 = [Tokens2 ++ [H2], []];
        _ -> L2 = [Tokens, Acc ++ [H]], T2 = T
    end,
    scan(T2, L2).

scan(P) -> [Tokens, _Acc] = scan(P, [[], []]), Tokens.

token(X) ->
    case X of
        [$(] -> {'(', 1};
        [$)] -> {')', 1};
        [$:] -> {':', 1};
        [$\s] -> {' ', 1};
        _ -> {atom, list_to_atom(X), 1}
    end.

tokenize([L]) -> [token(L), {'$end', 1}];
tokenize([H | T]) -> [token(H)] ++ tokenize(T).
