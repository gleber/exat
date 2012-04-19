%%
%%exat_app.erl
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
-module(exat_app).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export([start/0, stop/0]).

%%====================================================================
%% Internal exports
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

%%====================================================================
%% Records
%%====================================================================

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Func: start/0
%%====================================================================

start() ->
    application:start(inets, permanent),  % FIXME! Use ".rel"
    application:start(exat, permanent),
    case init:get_argument(start) of
        {ok, [[List]]} ->
            ApplicationList = string:tokens(List, ","),
            case ApplicationList of
                [] -> ok;
                _ ->
                    logger:log('AMS',
                               {"Staring Applications: ~s",
                                [lists:flatten(["{" ++ X ++ "}"
                                                || X <- ApplicationList])]}),
                    lists:foreach(fun (X) ->
                                          M = list_to_atom(X),
                                          io:format("~p~n", [M]),
                                          M:start()
                                  end,
                                  ApplicationList)
            end;
        _ -> ok
    end.

%%====================================================================
%% Func: stop/1
%% Returns: any
%%====================================================================

stop() -> application:stop(exat).

%%====================================================================
%% Internal functions
%%====================================================================
