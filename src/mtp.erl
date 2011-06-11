%
% mtp.erl
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
-module (mtp).

%%====================================================================
%% Include files
%%====================================================================
-include_lib("kernel/include/inet.hrl").
-include ("acl.hrl").
-include ("fipa_ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================
-export ([http/1,
          addresses/0,
          http_mtp_post/3,
          http_mtp_encode_and_send/3]).

-export ([init/1,
          handle_call/3,
          terminate/2,
          code_change/3]).

%%====================================================================
%% Macros
%%====================================================================
-define (BAD_RESPONSE, {400, "Bad Request",
                        [{"Content-Type", "text/plain"},
                         {"Cache-Control", "no-cache"},
                         {"Connection", "close"}],
                        ""}).

-define (OK_RESPONSE,  {200, "OK",
                        [{"Content-Type", "text/plain"},
                         {"Cache-Control", "no-cache"},
                         {"Connection", "close"}],
                        ""}).


%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Func: http/1
%%====================================================================
http (Port) ->
  logger:start ('HTTP-MTP'),
  logger:log ('HTTP-MTP', {"MTP Started at port ~w", [Port]}),
  {ok, Hostname} = inet:gethostname (),
  {ok, HostEnt} = inet:gethostbyname (Hostname),
  MTPAddress = lists:flatten (
                 ["http://", HostEnt#hostent.h_name, ":",
                  integer_to_list (Port), "/acc" ]),
  eresye:assert (agent_registry, {mtp_address, MTPAddress}),

  http_server:start (Port, {mtp, http_mtp_post}, []).

%%====================================================================
%% Func: addresses/0
%%====================================================================
addresses () ->
  MTPAddress = eresye:query_kb (agent_registry, {mtp_address, '_'}),
  [ X || {_,X} <- MTPAddress].

%%====================================================================
%% Func: http_mtp_post/3
%%====================================================================
http_mtp_post (Socket, ['POST', Url, Headers, Content], Params) ->
    %%io:format ("URL = ~s~n", [Url]),
    %%display_params (Headers),
    ContentLines = http_server:split_lines (Content),
    ContentType = http_server:get_key (Headers, "Content-Type"),
    {ok, [Encoding, Media, EncodingParams]} =
        http_server:media_type_decode (ContentType),
    http_mtp_decode (Encoding, Media, EncodingParams, ContentLines);

http_mtp_post (_, [_, _, _, _], _) -> ?BAD_RESPONSE.


%%
%%

%%====================================================================
%% Func: http_mtp_decode/3
%%====================================================================
http_mtp_decode ("multipart", _, EncodingParams, ContentLines) ->
    Boundary = http_server:get_key (EncodingParams, "boundary"),
    %%io:format ("Boundary ~s~n", [Boundary]),
    SplittedLines = http_server:multi_part_split (ContentLines,
                                                  "--" ++ Boundary),
    %%io:format ("----~nLines ~s~n----~n", [lists:flatten(ContentLines)]),
    [_, Envelope0, ACLMessage0 | _] = SplittedLines,
    {To, From, ACLRepr} =
        envelope:parse_xml_envelope (lists:flatten (
                                       mtp_utils:skip_to_empty_line (Envelope0))),
    ACLMessage = mtp_utils:replace_newlines ([], lists:flatten (
                                                   mtp_utils:skip_to_empty_line (ACLMessage0))),
    %%io:format ("To  = ~w~n", [To]),
    %%io:format ("ACL len = ~w~n", [length (ACLMessage)]),
    decode_and_forward_acl (To, From, ACLMessage, ACLRepr);

http_mtp_decode (_, _, _, _) -> ?BAD_RESPONSE.

%%
%%
decode_and_forward_acl (_, _, Message, "fipa.acl.rep.string.std") ->
    DecodedMessage = sl:decode (Message),
    %%io:format ("MSG = ~p~n", [DecodedMessage]),
    ACLMessage = list_to_tuple ([ aclmessage |
                                  tuple_to_list (DecodedMessage)]),
    %%io:format ("Message = ~w~n", [ACLMessage]),
    %%io:format ("R = ~w~n", [ACLMessage#aclmessage.receiver]),

    %% decode content
    ParsedMessage =
        case ontology_service:get_codec (ACLMessage#aclmessage.ontology) of
            {ok, Codec} ->
                {ok, SL} = sl:decode (ACLMessage#aclmessage.content,
                                      ascii_sl, erlang_sl),
                %%io:format ("Content = ~p~n", [SL]),
                ACLMessage#aclmessage { content = Codec:decode (SL) };
            _ -> ACLMessage
        end,


    %% determine receiver list
    Receivers =
        case is_list (ParsedMessage#aclmessage.receiver) of
            true ->
                ParsedMessage#aclmessage.receiver;
            false ->
                [ParsedMessage#aclmessage.receiver]
        end,
    CurrentPlatform = exat:current_platform(),
    %%io:format ("Receivers = ~w~n", [Receivers]),
    LocalReceivers = lists:filter (
                       fun (X) ->
                               {ID, HAP} =
                                   exat:split_agent_identifier (
                                     X#'agent-identifier'.name),
                               HAP == CurrentPlatform
                       end, Receivers),
    MessagesToSend = [ParsedMessage#aclmessage { receiver = X } ||
                         X <- lists:map (
                                fun (X) ->
                                        {ID, _} =
                                            exat:split_agent_identifier (
                                              X#'agent-identifier'.name),
                                        X#'agent-identifier' { name = ID }
                                end, LocalReceivers)],
    %%io:format ("Parsed Message = ~w~n", [MessagesToSend]),
    lists:foreach (
      fun (X) ->
              Receiver = (X#aclmessage.receiver)#'agent-identifier'.name,
              {ID, _} = exat:split_agent_identifier (Receiver),
              %%io:format ("Recv = ~w~n", [Receiver]),
              gen_server:call(list_to_atom (ID), [acl_erl_native, X])
      end, MessagesToSend),
    ?OK_RESPONSE;

decode_and_forward_acl (_, _, _, _) -> ?BAD_RESPONSE.


display_params ([]) -> ok;
display_params ([{Param, Value} | T]) ->
  io:format ("~s = ~s~n", [Param, Value]),
  display_params (T).


%%====================================================================
%% Func: http_mtp_encode_and_send/1
%%====================================================================
http_mtp_encode_and_send (To, From, Message) ->
%%io:format ("Message ~w~n", [Message]),
    XX = fipa_ontology_sl_codec:encode (Message),
%%io:format ("XX ~w~n", [XX]),
    ACL = sl:encode (XX),
%%io:format ("ACL ~s~n", [ACL]),

    Envelope = envelope:make_xml_envelope (To, From, length (ACL)),
%%io:format ("Envelope ~s~n", [Envelope]),

    HTTPBody = lists:flatten (
                 [[$\r, $\n],
                  "This is not part of MIME multipart",  [$\r, $\n,  $\r, $\n],
                  "--251D738450A171593A1583EB", [$\r, $\n],
                  "Content-Type: application/xml",  [$\r, $\n,  $\r, $\n],
                  Envelope,  [$\r, $\n,  $\r, $\n],
                  "--251D738450A171593A1583EB", [$\r, $\n],
                  "Content-Type: application/text",  [$\r, $\n,  $\r, $\n],
                  ACL,  [$\r, $\n],
                  "--251D738450A171593A1583EB--", [$\r, $\n, $\r, $\n]]),

    [ReceiverAddr | _] = To#'agent-identifier'.addresses,

    {http, _, Host, Port, _, _} = http_uri:parse (ReceiverAddr),

    Headers = [ {"Cache-Control", "no-cache"},
                {"Mime-Version", "1.0"},
                {"Host", Host ++ ":" ++ integer_to_list (Port)},
                {"Content-Length", integer_to_list (length (HTTPBody))},
                {"Connection", "close"}],

%%display_params (Headers),
%%io:format ("Body ~w~n", [HTTPBody]),

%%io:format ("Receiver ~w,~w~n",
%%           [(Message#aclmessage.receiver)#'agent-identifier'.name,
%%            ReceiverAddr]),

    Request = { ReceiverAddr,
                Headers,
                "multipart/mixed ; boundary=\"251D738450A171593A1583EB\"",
                HTTPBody},

%%   HTTPID = http:request (
%%              post,
%%              { ReceiverAddr,
%%                Headers,
%%                "multipart/mixed ; boundary=\"251D738450A171593A1583EB\"",
%%                HTTPBody}, [], [{sync, true}]),

    HTTPID = gen_server:call (mtp_sender, {http_post, Request}),

    {ok, RequestID} = HTTPID,
    Result = RequestID,
%%io:format ("Result ~p, ~p~n", [self (), Result]),
    {StatusLine, ReceivedHeaders, ReceivedBody} = Result,
    {HttpVersion, StatusCode, ReasonPhrase} = StatusLine.
  %%io:format ("Status ~w,~s~n", [StatusCode, ReasonPhrase]),
  %%io:format ("Body ~s~n", [ReceivedBody]).



%%====================================================================
%% Func: init/1
%%====================================================================
init (_) -> {ok, []}.

%%====================================================================
%% Func: handle_call/3
%%====================================================================
handle_call({http_post, Request}, From, State) ->
  HTTPID = httpc:request(post, Request, [], [{sync, true}]),
  {reply, HTTPID, State}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate(_, _) -> ok.


%%====================================================================
%% Func: code_change/3
%%====================================================================
code_change(OldVsn, State, Extra) -> {ok, State}.

