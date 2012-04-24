%%
%% envelope.erl
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
-module(envelope).

-include_lib("xmerl/include/xmerl.hrl").

-include("acl.hrl").

-include("fipa_ontology.hrl").

-export([make_xml_envelope/3, make_xml_envelope/4,
         parse_xml_envelope/1, test/0]).

make_xml_envelope(To, From, Length, AclRepr) ->
    XmlTo = {'agent-identifier', [],
             [{name, [],
               [binary_to_list(iolist_to_binary(To#'agent-identifier'.name))]},
              {addresses, [],
               [{url, [], [binary_to_list(X)]}
                || X <- To#'agent-identifier'.addresses]}]},
    XmlFrom = {'agent-identifier', [],
               [{name, [],
                 [binary_to_list(iolist_to_binary(From#'agent-identifier'.name))]},
                {addresses, [],
                 [{url, [], [binary_to_list(X)]}
                  || X <- From#'agent-identifier'.addresses]}]},
    Envelope = {envelope, [],
                [{params, [{index, 1}],
                  [{to, [], [XmlTo]}, {from, [], [XmlFrom]},
                   {'acl-representation', [], [AclRepr]},
                   {'payload-length', [], [integer_to_list(Length)]},
                   {'payload-encoding', [], ["US-ASCII"]},
                   {'intended-receiver', [], [XmlTo]}]}]},
    lists:flatten(xmerl:export_simple([Envelope],
                                      xmerl_xml)).

make_xml_envelope(To, From, Length) ->
    make_xml_envelope(To, From, Length,
                      "fipa.acl.rep.string.std").

get_xml_element(XML = #xmlElement{name = Key}, Key) ->
    {ok, XML};
get_xml_element(XML, Key)
  when is_record(XML, xmlElement) ->
    get_xml_element(XML#xmlElement.content, Key);
get_xml_element([], _) -> {error, nil};
get_xml_element([XMLH | XMLT], Key) ->
    case get_xml_element(XMLH, Key) of
        {ok, Data} -> {ok, Data};
        _ -> get_xml_element(XMLT, Key)
    end;
get_xml_element(_, _) -> {error, nil}.

get_xml_text(XML) ->
    [Content | _] = XML#xmlElement.content,
    Text = Content#xmlText.value,
    iolist_to_binary(Text).

parse_xml_envelope(XmlEnvelope) ->
    {Envelope, _} = xmerl_scan:string(XmlEnvelope),
    {ok, XTo} = get_xml_element(Envelope, to),
    {ok, XToAgentID} = get_xml_element(XTo, 'agent-identifier'),
    {ok, XToAgentName} = get_xml_element(XToAgentID, name),
    {ok, XToAgentAddress} = get_xml_element(XToAgentID,
                                            addresses),
    {ok, XToURL} = get_xml_element(XToAgentAddress, url),
    ToAgentName = get_xml_text(XToAgentName),
    ToURL = get_xml_text(XToURL),
    {ok, XFrom} = get_xml_element(Envelope, from),
    {ok, XFromAgentID} = get_xml_element(XFrom,
                                         'agent-identifier'),
    {ok, XFromAgentName} = get_xml_element(XFromAgentID,
                                           name),
    {ok, XFromAgentAddress} = get_xml_element(XFromAgentID,
                                              addresses),
    {ok, XFromURL} = get_xml_element(XFromAgentAddress,
                                     url),
    FromAgentName = get_xml_text(XFromAgentName),
    FromURL = get_xml_text(XFromURL),
    {ok, XACLRepresentation} = get_xml_element(Envelope,
                                               'acl-representation'),
    ACLRepresentation = get_xml_text(XACLRepresentation),
                                                %io:format ("From ~s[~s]~n", [FromAgentName, FromURL]),
                                                %io:format ("To   ~s[~s]~n", [ToAgentName, ToURL]),
                                                %io:format ("Repr ~s~n", [ACLRepresentation]),
    {#'agent-identifier'{name = ToAgentName,
                         addresses = [ToURL]},
     #'agent-identifier'{name = FromAgentName,
                         addresses = [FromURL]},
     ACLRepresentation}.

test() ->
    make_xml_envelope(#'agent-identifier'{name = pippo,
                                          addresses = "http://a:80/acc"},
                      #'agent-identifier'{name = pluto,
                                          addresses = "http://b:80/acc"},
                      100).
