%%
%%mtp.erl
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
-module(mtp).

%%====================================================================
%% Include files
%%====================================================================
-include_lib("kernel/include/inet.hrl").

-include("acl.hrl").

-include("fipa_ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================
-export([addresses/0,
         http_mtp_encode_and_send/3]).

%%====================================================================
%% Macros
%%====================================================================

%%====================================================================
%% External functions
%%====================================================================

%%====================================================================
%% Func: addresses/0
%%====================================================================
addresses() ->
    MTPAddress = seresye:query_kb(agent_registry,
                                  {mtp_address, '_'}),
    [ X || {_, X} <- MTPAddress].

%%====================================================================
%% Func: http_mtp_encode_and_send/1
%%====================================================================
http_mtp_encode_and_send(To, From, Message) ->
    XX = fipa_ontology_sl_codec:encode(Message),
    %% io:format ("XX ~w~n", [XX]),
    ACL = sl:encode(XX),
    %% io:format ("ACL ~s~n", [ACL]),
    Envelope = envelope:make_xml_envelope(To, From,
                                          length(ACL)),
    %%io:format ("Envelope ~s~n", [Envelope]),
    HTTPBody = lists:flatten([[$\r, $\n],
                              "This is not part of MIME multipart",
                              [$\r, $\n, $\r, $\n],
                              "--251D738450A171593A1583EB", [$\r, $\n],
                              "Content-Type: application/xml",
                              [$\r, $\n, $\r, $\n], Envelope,
                              [$\r, $\n, $\r, $\n],
                              "--251D738450A171593A1583EB", [$\r, $\n],
                              "Content-Type: application/text",
                              [$\r, $\n, $\r, $\n], ACL, [$\r, $\n],
                              "--251D738450A171593A1583EB--",
                              [$\r, $\n, $\r, $\n]]),
    [ReceiverAddr0 | _] = To#'agent-identifier'.addresses,
    ReceiverAddr = binary_to_list(ReceiverAddr0),
    {Host, Port} = case http_uri:parse(ReceiverAddr) of
                       {http, _, H, P, _, _} -> {H, P};
                       {ok, {http, _, H, P, _, _}} -> {H, P}
                   end,
    Headers = [{"Cache-Control", "no-cache"},
               {"Mime-Version", "1.0"},
               {"Host", Host ++ ":" ++ integer_to_list(Port)},
               {"Content-Length", integer_to_list(length(HTTPBody))},
               {"Connection", "close"}],
    %%display_params (Headers),
    %%io:format ("Body ~w~n", [HTTPBody]),
    %%io:format ("Receiver ~w,~w~n",
    %%          [(Message#aclmessage.receiver)#'agent-identifier'.name,
    %%           ReceiverAddr]),
    Request = {ReceiverAddr, Headers,
               "multipart/mixed ; boundary=\"251D738450A17159"
               "3A1583EB\"",
               HTTPBody},
    %%  HTTPID = http:request (
    %%             post,
    %%             { ReceiverAddr,
    %%               Headers,
    %%               "multipart/mixed ; boundary=\"251D738450A171593A1583EB\"",
    %%               HTTPBody}, [], [{sync, true}]),
    case httpc:request(post, Request, [], [{sync, true}]) of
        {ok, RequestID} ->
            Result = RequestID,
            {StatusLine, _ReceivedHeaders, _ReceivedBody} = Result,
            {_HttpVersion, _StatusCode, _ReasonPhrase} = StatusLine;
        _ ->
            ok
    end.
