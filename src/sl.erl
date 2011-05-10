%
% sl.erl
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
-module (sl).
%%====================================================================
%% Include files
%%====================================================================
-include ("acl.hrl").
-include ("ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================

-export ([decode/1,
          decode/3,
          encode/1,
          isList/1,
          isString/1,
          get_slot/2,
          replace_slot/3]).


%%====================================================================
%% Func: decode/1
%%====================================================================
decode (Message) ->
  decode (Message, ascii_sl, ontology).

%%====================================================================
%% Func: decode/3
%%====================================================================
decode (AsciiMessage, ascii_sl, erlang_sl) ->
  %%io:format ("ASCII ~s~n", [AsciiMessage]),
  T = tokenize (AsciiMessage),
  sl_parser:parse (T);
decode (SLMessage, erlang_sl, ontology) ->
  fipa_ontology_sl_codec:decode (SLMessage);
decode (AsciiMessage, ascii_sl, ontology) ->
  {ok, ErlangSL} = decode (AsciiMessage, ascii_sl, erlang_sl),
  %%io:format ("~w~n", [ErlangSL]),
  decode (ErlangSL, erlang_sl, ontology).



%%====================================================================
%% Func: encode/1
%% Description: ErlangSLForm --> ASCII SL Form
%% Returns: ASCIIForm
%%====================================================================
encode (Message) ->
  lists:flatten (encode (Message, isList (Message))).


encode (X, true) -> % isList
  [ "(", encode_list_terms ([], X), ")" ];
encode ({_, ?ACL_ANY}, false) -> % is not List
  [ ];
encode ({SlotName, SlotValue}, false) -> % is not List
  [ ":", atom_to_list (SlotName), " ",
    encode  (SlotValue, isList (SlotValue)) , " " ];
encode (TermValue, false) -> % is String
  [TermValue, " "].


encode_list_terms (Acc, []) -> lists:reverse (Acc);
encode_list_terms (Acc, [H|T]) ->
  Encoding = encode (H, isList (H)),
  encode_list_terms ([Encoding | Acc], T).


%%====================================================================
%% Func: get_slot/2
%% Description: returns the value of a given slot name
%% Returns: Value | ?ACL_ANY
%%====================================================================
get_slot (Key, List) ->
  case lists:keysearch (Key, 1, List) of
    {value, {_, Value}} -> Value;
    _ -> ?ACL_ANY
  end.

%%====================================================================
%% Func: replace_slot/3
%% Description: changes the value of a given slot name
%% Returns: None
%%====================================================================
replace_slot (Key, List, NewValue) ->
  lists:keyreplace (Key, 1, List, {Key, NewValue}).

%%====================================================================
%% Func: isList/1
%% Description: Checks if the given data is a list of strings
%% Returns: true | false
%%====================================================================
isList ([]) -> false;
isList ([H | _]) when is_number (H) -> false;
isList ([H | _]) when is_list (H) -> true;
isList (X) when is_tuple (X) -> false;
isList (_) -> true.


%%====================================================================
%% Func: isString/1
%% Description: Checks if the given data is a strings
%% Returns: true | false
%%====================================================================
isString ([]) -> true;
isString ([H | T]) when is_number (H) -> isString (T);
isString (X) when is_tuple (X) -> false;
isString (_) -> false.


%%====================================================================
%% Internal functions
%%====================================================================

tokenize ([]) -> [{'$end',1}];
tokenize ([H|T]) -> [ {list_to_atom ([H]),1} | tokenize (T) ].


