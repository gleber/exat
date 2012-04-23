%%
%%ontology_service.erl
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
-module(ontology_service).

-behaviour(gen_server).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================
-export([get_codec/1, register_codec/2]).

-export([code_change/3, handle_call/3, handle_info/2, handle_cast/2,
         init/1, terminate/2]).

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Function: register_codec/2
%% Description: registers a new ontology codec
%%====================================================================
register_codec(OntologyName, Codec) ->
    gen_server:call(ontology_service,
                    {register, OntologyName, Codec}).

%%====================================================================
%% Function: register_codec/2
%% Description: get a codec
%% Returns: {ok, CodecName} |
%%          error
%%====================================================================
get_codec(OntologyName) ->
    gen_server:call(ontology_service,
                    {get_codec, OntologyName}).

%%====================================================================
%% Function: init/1
%% Description: Intializes the server
%%====================================================================
init(_) ->
    logger:start('ONTOLOGY'),
    logger:log('ONTOLOGY', "Started"),
    {ok, dict:new()}.

%%====================================================================
%% Function: handle_call/3
%% Description: handles the requests
%%====================================================================
handle_call({register, OntologyName, Codec}, _, Dict) ->
    logger:log('ONTOLOGY',
               {"Registering Codec ~s for ontology ~s",
                [Codec, OntologyName]}),
    {reply, ok, dict:store(OntologyName, Codec, Dict)};
handle_call({get_codec, OntologyName}, _, Dict) ->
    Result = dict:find(OntologyName, Dict),
    case {Result, OntologyName} of
        {error, '_'} -> ok;
        {error, _} ->
            logger:log('ONTOLOGY',
                       {"Codec not found for ontology ~s", [OntologyName]}),
            ok;
        _ -> ok
    end,
    {reply, Result, Dict}.

%%====================================================================
%% Function: handle_info/2
%%====================================================================
handle_info(Any, State) -> 
    {stop, {unknown_info, Any}, State}.

handle_cast(Call, State) ->
    {stop, {unknown_cast, Call}, State}.

%%====================================================================
%% Function: terminate/2
%%====================================================================
terminate(_, _) -> ok.

%%====================================================================
%% Function: code_change/3
%%====================================================================
code_change(_OldVsn, State, _Extra) -> {ok, State}.
