%
% acl.erl
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
-module (acl).
-export ([parse_message/1,
          sl0_getcontent/1,
          sl0_parse/1,
          sl0_parsecontent/1,
          sl0_getslot/2,
          erlang_to_sl0/1,

          sendacl/1,
          inform/1,
          ask_if/1,
          call_for_proposal/1,
          propose/1,
          accept_proposal/1,
          reject_proposal/1,
          refuse/1,
          request/1,
          query_if/1,
          query_ref/1,
          reply/3,

          ensure_list/1,
          refinepattern/2,
          refinepattern/4,
          hexlify/1,
          unhexlify/1]).

-author ('csanto@diit.unict.it').
-include ("object.hrl").
-include ("acl.hrl").
-include ("fipa_ontology.hrl").

-define (ACL_REPRESENTATION, acl_erl_native).




erl_to_sl0 ([], Acc) ->
  lists:flatten (lists:reverse ([ ")" | Acc]));

erl_to_sl0 ([{Slotname, SlotValue} | Tail], Acc) ->
  erl_to_sl0 (Tail,
              [ " ", erlang_to_sl0 (SlotValue), " " ,
                atom_to_list (Slotname), ":"
                | Acc]);

erl_to_sl0 ([Head | Tail], Acc) ->
  ZHead = if
            is_atom (Head) -> atom_to_list (Head);
            is_list (Head) -> erlang_to_sl0 (Head);
            true -> Head
          end,
  erl_to_sl0 (Tail, [ " " , ZHead | Acc]);

erl_to_sl0 (Value, Acc) when is_atom (Value) ->
  atom_to_list (Value).


%%%
erlang_to_sl0 (ErlangSentence) when is_atom (ErlangSentence) ->
  atom_to_list (ErlangSentence);
erlang_to_sl0 ([$#, $s, $# | Tail]) ->
  Tail;
erlang_to_sl0 (ErlangSentence) ->
  erl_to_sl0 (ErlangSentence, ["("]).


%%%
sl0_parse (M) ->
  S = aclscanner:scan (M),
  T = aclscanner:tokenize (S),
  aclparser:parse (T).
%% returns {error, ErrorCond} or {ok, Terms}

sl0_getslot (SearchedSlotname, []) -> ?ACL_ANY;
sl0_getslot (SearchedSlotname, [{SlotName, SlotValue} | MessageTail])
when SlotName == SearchedSlotname ->
  SlotValue;
sl0_getslot (SearchedSlotname, [_ | MessageTail]) ->
  sl0_getslot (SearchedSlotname, MessageTail).

sl0_getcontent (M) ->
        atom_to_list (sl0_getslot (content, M)).


islowercase ([]) -> true;
islowercase ([H|T]) -> islowercase (H) and islowercase (T);
islowercase (H) when not is_list(H) -> (H >= $a) and (H =< $z).

tolower (H) -> H + 32.

lowcase ([]) -> [];
lowcase ([H | T]) when (H >= $A) and (H =< $Z) ->  [tolower (H) | lowcase (T)];
lowcase ([H | T]) ->  [H | lowcase (T)].



parse_message (M) ->
  {ok, [SpeechAct | Slots]} = sl0_parse (M),
  LSpeechAct = list_to_atom (lowcase (atom_to_list (SpeechAct))),
  Message = #aclmessage { speechact = LSpeechAct,
                          sender = translate_agent_identifier (
                                     sl0_getslot (sender, Slots)),
                          receiver = translate_receiver_agent_identifier (
                                       sl0_getslot (receiver, Slots)),
                          'reply-to' = sl0_getslot ('reply-to', Slots),
                          content = sl0_getslot (content, Slots),
                          language = sl0_getslot (language, Slots),
                          encoding = sl0_getslot (encoding, Slots),
                          ontology = sl0_getslot (ontology, Slots),
                          protocol = sl0_getslot (protocol, Slots),
                          'conversation-id' = sl0_getslot ('conversation-id',
                                                           Slots),
                          'reply-with' = sl0_getslot ('reply-with', Slots),
                          'in-reply-to' = sl0_getslot ('in-reply-to', Slots),
                          'reply-by' = sl0_getslot ('reply-by',Slots)
                         },
  Message.


sl0_parsecontent (M) ->
  sl0_parse (ensure_list (M#aclmessage.content)).


translate_agent_identifier (ID = ['agent-identifier' | _]) ->
  sl_to_erlang_agent_identifier (ID);
translate_agent_identifier (ID) -> ID.

translate_receiver_agent_identifier (['set' | Set]) ->
  FIPA_ID_Set = [ sl_to_erlang_agent_identifier (X) || X <- Set];
translate_receiver_agent_identifier (ID) ->
  translate_agent_identifier (ID).

sl_to_erlang_agent_identifier (['agent-identifier',
                                {name, Name},
                                {addresses, Addresses}]) ->
  [Adr | _] = sl_sequence_to_erlang (Addresses),
  #'agent-identifier' { name = Name, addresses = Adr}.


erlang_to_erlang_sl_agent_identifier (AgentIdentifier)
  when record (AgentIdentifier, 'agent-identifier') ->
  ['agent-identifier',
   {name, AgentIdentifier#'agent-identifier'.name},
   {addresses, ['sequence',
                AgentIdentifier#'agent-identifier'.addresses]}];
erlang_to_erlang_sl_agent_identifier (AgentIdentifier) ->
  erl_sl0_to_atom (AgentIdentifier).


sl_sequence_to_erlang (['sequence' | X]) -> X.

to_list (X) when is_atom (X) -> atom_to_list (X);
to_list (X) -> X.

ensure_list (X) -> to_list (X).

erl_sl0_to_atom (X) when is_list (X) -> list_to_atom (erlang_to_sl0 (X));
erl_sl0_to_atom (X) -> X.


remove_nihils ([], Acc) ->
  lists:reverse (Acc);

remove_nihils ([{SlotName, ?ACL_ANY} | Tail], Acc) ->
  remove_nihils (Tail, Acc);

remove_nihils ([{SlotName, SlotValue} | Tail], Acc) ->
  remove_nihils (Tail, [{SlotName, SlotValue} | Acc]).

remove_nihils (List) ->
  remove_nihils (List, []).


%
% Speech Act Library
%

% if the ID is an object, get the associated agent
encode_agent_identifier (Identifier) when record (Identifier, object) ->
  encode_agent_identifier (object:agentof (Identifier));

% if the ID is an atom (i.e. agent/process name), transform it into a string
encode_agent_identifier (Identifier) when is_atom (Identifier) ->
  encode_agent_identifier (atom_to_list (Identifier));

% if the ID is a string, transform it into a #agent-identifier
encode_agent_identifier (Identifier) when is_list (Identifier) ->
  case sl:isString (Identifier) of
    true ->
      {ID, HAP} = exat:split_agent_identifier (Identifier),
      #'agent-identifier' { name = lists:flatten ([ID, "@", HAP]),
                            addresses = mtp:addresses ()};
    false ->
      [ encode_agent_identifier (X) || X <- Identifier]
  end;

encode_agent_identifier (Default) -> Default.




sendacl (Message) ->
  Sender = encode_agent_identifier (Message#aclmessage.sender),
  R = encode_agent_identifier (Message#aclmessage.receiver),
  Receivers =
    if
      is_list (R) -> R;
      true -> [R]
    end,

  %%io:format ("Sender ~w~n", [Sender]),
  %%io:format ("Receivers ~w~n", [Receivers]),

  %% encode content
  EncodedContent = "\"" ++
    case ontology_service:get_codec (Message#aclmessage.ontology) of
      {ok, Codec} ->
        SL = Codec:encode (Message#aclmessage.content),
        sl:encode (SL);
      _ -> Message#aclmessage.content
    end ++ "\"",

  [_ | Temp] = tuple_to_list (Message#aclmessage {
                                sender = Sender,
                                receiver = Receivers,
                                content = EncodedContent }),
  TransformedMessage = list_to_tuple (Temp),
  [mtp:http_mtp_encode_and_send (X, Sender, TransformedMessage)
   || X <- Receivers],
  ok.


inform (Message) ->
  sendacl (Message#aclmessage {speechact = 'INFORM'}).

ask_if (Message) ->
  sendacl (Message#aclmessage {speechact = 'ASK-IF'}).

call_for_proposal (Message) ->
  sendacl (Message#aclmessage {speechact = 'CFP'}).

propose (Message) ->
  sendacl (Message#aclmessage {speechact = 'PROPOSE'}).

accept_proposal (Message) ->
  sendacl (Message#aclmessage {speechact = 'ACCEPT-PROPOSAL'}).

reject_proposal (Message) ->
  sendacl (Message#aclmessage {speechact = 'REJECT-PROPOSAL'}).

refuse (Message) ->
  sendacl (Message#aclmessage {speechact = 'REFUSE'}).

request (Message) ->
  sendacl (Message#aclmessage {speechact = 'REQUEST'}).

query_if (Message) ->
  sendacl (Message#aclmessage {speechact = 'QUERY-IF'}).

query_ref (Message) ->
  sendacl (Message#aclmessage {speechact = 'QUERY-REF'}).

reply (Request, NewSpeechAct, NewContent) ->
  Receiver =
    if
      Request#aclmessage.'reply-to' == ?ACL_ANY ->
        Request#aclmessage.sender;
      true ->
        Request#aclmessage.'reply-to'
    end,
  sendacl (Request#aclmessage
           {speechact = NewSpeechAct,
            sender = Request#aclmessage.receiver,
            receiver = Receiver,
            'in-reply-to' = Request#aclmessage.'reply-with',
            content = NewContent}).

%
% pattern library
%
unify_atoms (?ACL_ANY, ?ACL_ANY) -> ?ACL_ANY;
unify_atoms (A1, ?ACL_ANY) -> A1;
unify_atoms (?ACL_ANY, A2) -> A2;
unify_atoms (A1, A2) -> A2.

unify_patterns ([], [], Acc) ->
  lists:reverse (Acc);
unify_patterns ( [H1 | T1], [H2 | T2] , Acc) ->
  unify_patterns (T1, T2, [ unify_atoms (H1, H2) | Acc]).

refinepattern (BasePattern, RefiningPattern) ->
  LBasePattern = tuple_to_list (BasePattern),
  LRefiningPattern = tuple_to_list (RefiningPattern),
  L1 = length (LBasePattern),
  L2 = length (LRefiningPattern),
  if
    L1 =/= L2 -> exit ({badarg,'pattern lengths differ'});
    true ->
      list_to_tuple (unify_patterns (LBasePattern, LRefiningPattern, []))
  end.


refinepattern (Object, PatternName, Index, RefiningPattern) ->
  PatternList = object:super (Object, pattern, [PatternName]),
  lists:sublist (PatternList, Index - 1) ++
    [refinepattern (lists:nth (Index, PatternList), RefiningPattern)] ++
    lists:nthtail (Index, PatternList).


%
% acl utils
%
hexlify ([]) -> "";
hexlify ([H | T]) ->
  %[Z | _] = io_lib:format ("~.16B", [H]),
  hexof (H) ++ hexlify (T).

unhexlify ([]) -> "";
unhexlify ([H1, H2 | T]) ->
  Int = hexdigit (H1) * 16 + hexdigit (H2),
  [Int | unhexlify (T)].

hexof (X) ->
  Z = tohex (X div 16) ++ tohex (X rem 16),
  Z.

tohex (0) -> "0";
tohex (1) -> "1";
tohex (2) -> "2";
tohex (3) -> "3";
tohex (4) -> "4";
tohex (5) -> "5";
tohex (6) -> "6";
tohex (7) -> "7";
tohex (8) -> "8";
tohex (9) -> "9";
tohex (10) -> "A";
tohex (11) -> "B";
tohex (12) -> "C";
tohex (13) -> "D";
tohex (14) -> "E";
tohex (15) -> "F".

hexdigit ($0) -> 0;
hexdigit ($1) -> 1;
hexdigit ($2) -> 2;
hexdigit ($3) -> 3;
hexdigit ($4) -> 4;
hexdigit ($5) -> 5;
hexdigit ($6) -> 6;
hexdigit ($7) -> 7;
hexdigit ($8) -> 8;
hexdigit ($9) -> 9;
hexdigit ($A) -> 10;
hexdigit ($B) -> 11;
hexdigit ($C) -> 12;
hexdigit ($D) -> 13;
hexdigit ($E) -> 14;
hexdigit ($F) -> 15.
