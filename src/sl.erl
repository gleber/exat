%%
%% sl.erl
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
-module(sl).

%%====================================================================
%% Include files
%%====================================================================
-include("acl.hrl").

-include("ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================

-export([decode/1, decode/3, encode/1, get_slot/2, replace_slot/3]).

%%====================================================================
%% Func: decode/1
%%====================================================================
decode(Message) -> decode(Message, ascii_sl, erlang_sl).

%%====================================================================
%% Func: decode/3
%%====================================================================
decode(AsciiMessage, ascii_sl, erlang_sl) ->
    T = tokenize(AsciiMessage),
    sl_parser:parse(T).

%%====================================================================
%% Func: encode/1
%% Description: ErlangSLForm --> ASCII SL Form
%% Returns: ASCIIForm
%%====================================================================
encode(Message) ->
    binary_to_list(iolist_to_binary(encode0(Message))).

encode0(X) when is_list(X) ->
    ["(", encode_list_terms([], X), ")"];
encode0(X) when is_binary(X) -> % is String
    [X];
encode0({_, ?ACL_ANY}) ->
    [];
encode0({SlotName, SlotValue}) ->
    [":", atom_to_list(SlotName), " ", encode0(SlotValue)].

encode_list_terms(Acc, [Last]) -> 
    lists:reverse(Acc, [encode0(Last)]);
encode_list_terms(Acc, [H | T]) ->
    Encoding = encode0(H),
    encode_list_terms([" ", Encoding | Acc], T).

%%====================================================================
%% Func: get_slot/2
%% Description: returns the value of a given slot name
%% Returns: Value | ?ACL_ANY
%%====================================================================
get_slot(Key, List) ->
    case lists:keysearch(Key, 1, List) of
        {value, {_, Value}} -> Value;
        _ -> ?ACL_ANY
    end.

%%====================================================================
%% Func: replace_slot/3
%% Description: changes the value of a given slot name
%% Returns: None
%%====================================================================
replace_slot(Key, List, NewValue) ->
    lists:keyreplace(Key, 1, List, {Key, NewValue}).


%%====================================================================
%% Internal functions
%%====================================================================

tokenize([]) -> [{'$end', 1}];
tokenize([H | T]) ->
    [{list_to_atom([H]), 1} | tokenize(T)].
