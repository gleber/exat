%%
%%exat_mtp_server.erl
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
-module(exat_mtp_server).

-behaviour(gen_server).
-behaviour(cowboy_http_handler).

%%====================================================================
%% Include files
%%====================================================================

-include_lib("kernel/include/inet.hrl").

-include("acl.hrl").
-include("fipa_ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% cowboy_http_handler callbacks
-export([init/3, handle/2, terminate/3]).

%%====================================================================
%% Internal exports
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

-define(BAD_RESPONSE, Req:respond(400)).

-define(OK_RESPONSE, Req:ok("")).

%%====================================================================
%% Records
%%====================================================================

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Function: start_link/1
%% Description: Starts the MTP worker
%%====================================================================

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% cowboy_http_handler callbacks

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, 
                                  [{<<"content-type">>, <<"text/plain">>}], 
                                  <<"Hello World!">>, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Func: http_mtp_post/3
%%====================================================================
handle_http(Req) ->
    http_mtp_post(Req:get(method),
                  Req:resource([lowercase, urldecode]), Req:get(headers),
                  Req).

http_mtp_post('POST', _Url, _Headers, Req) ->
    %%io:format ("URL = ~s~n", [Url]),
    %%display_params (Headers),
    Content = Req:parse_post(),
    [{part, _, XmlContent}] = [V1
                               || V1 <- Content, http_mtp_post_1(V1)],
    [{part, _, SLContent}] = [V2
                              || V2 <- Content, http_mtp_post_2(V2)],
    http_mtp_decode(Req, XmlContent, SLContent).

http_mtp_post_1({part, H, _D}) ->
    "application/xml" ==
        misultin_utility:header_get_value('Content-Type', H).

http_mtp_post_2({part, H, _D}) ->
    "application/text" ==
        misultin_utility:header_get_value('Content-Type', H).

%%
%%


%%====================================================================
%% Func: http_mtp_decode/3
%%====================================================================
http_mtp_decode(Req, XmlContent, SLContent) ->
    {To, From, ACLRepr} =
        envelope:parse_xml_envelope(binary_to_list(XmlContent)),
    decode_and_forward_acl(Req, To, From, SLContent,
                           ACLRepr).

%%
%%
decode_and_forward_acl(Req, _, _, Message,
                       <<"fipa.acl.rep.string.std">>) ->
    ACLMessage = acl:parse_message(binary_to_list(Message)),
    %%io:format ("Message = ~w~n", [ACLMessage]),
    %%decode content
    ParsedMessage = case ontology_service:get_codec(ACLMessage#aclmessage.ontology) of
                        {ok, Codec} ->
                            {ok, SL} = sl:decode(binary_to_list(ACLMessage#aclmessage.content)),
                            %%io:format ("Content = ~p~n", [SL]),
                            ACLMessage#aclmessage{content = Codec:decode(SL)};
                        _ -> ACLMessage
                    end,
    %%determine receiver list
    Receivers = case is_list(ParsedMessage#aclmessage.receiver) of
                    true -> ParsedMessage#aclmessage.receiver;
                    false -> [ParsedMessage#aclmessage.receiver]
                end,
    CurrentPlatform = exat:current_platform(),
    %%io:format ("Receivers = ~w~n", [Receivers]),
    LocalReceivers = [V1 || V1 <- Receivers,
                            is_local(V1, CurrentPlatform)],
    MessagesToSend = [ ParsedMessage#aclmessage{receiver = X} || X <- LocalReceivers],
    LocalAms = agent:full_local_name("ams"),
    %%io:format ("Message = ~p~n", [MessagesToSend]),
    lists:foreach(fun (X) ->
                          AgentName = (X#aclmessage.receiver)#'agent-identifier'.name,
                          Receiver = case binary_to_atom(AgentName, utf8) of
                                         LocalAms ->
                                             ams;
                                         ReceiverB -> ReceiverB
                                     end,
                          %%{ID, _} = exat:split_agent_identifier(Receiver),
                          %%io:format ("Recv = ~w~n", [Receiver]),
                          case whereis(Receiver) of
                              undefined ->
                                  case ams:get_registered_agents(Receiver) of
                                      [] ->
                                          io:format("There isn't agent ~p~n", [Receiver]);
                                      [#'agent-identifier'{name = Receiver, addresses = Addrs} = Receiver2 | _ ] ->
                                          X2 = X#aclmessage{receiver = Receiver2#'agent-identifier'{name=AgentName}},
                                          %%spawn(fun() -> acl:sendacl(X2) end)
                                          gen_server:call({via, proc_mobility, Receiver}, [acl_erl_native, X2])
                                  end;
                              _ ->
                                  gen_server:call(Receiver, [acl_erl_native, X])
                          end
                  end, MessagesToSend),
    ?OK_RESPONSE;
decode_and_forward_acl(Req, _, _, _, _) ->
    ?BAD_RESPONSE.

is_local(X, CurrentPlatform) ->
    {_ID, HAP} = exat:split_agent_identifier(X#'agent-identifier'.name),
    HAP == CurrentPlatform.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-record(state, {}).

init([Port]) ->
    process_flag(trap_exit, true),
    logger:start('HTTP-MTP'),
    logger:log('HTTP-MTP',
               {"MTP Starting at port ~w", [Port]}),
    {ok, Hostname} = inet:gethostname(),
    {ok, HostEnt} = inet:gethostbyname(Hostname),
    MTPAddress = iolist_to_binary(
                   lists:flatten(["http://",
                                  HostEnt#hostent.h_name, ":",
                                  integer_to_list(Port), "/acc"])),
    seresye:assert(agent_registry,
                   {mtp_address, MTPAddress}),
    logger:log('HTTP-MTP', {"Handing things over to cowboy", []}),
    Dispatch = cowboy_router:compile(
                 [
                  %% {URIHost, list({URIPath, Handler, Opts})}
                  {'_', [{'_', exat_mtp_server, []}]}
                 ]),
    %% Name, NbAcceptors, TransOpts, ProtoOpts
    cowboy:start_http(exat_mtp_server_listener, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),
    %% misultin:start_link([{port, Port},
    %%                      {acceptors_poolsize, 1},
    %%                      {loop, fun handle_http/1}])
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(exat_mtp_server_listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
