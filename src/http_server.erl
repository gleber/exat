%
% http_server.erl
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
-module (http_server).
-export ([start/0,
          start/2,
          start/3,
          start_sync/3,
          main_server/3,
          process_request/3,
          get_key/2,
          split_lines/1,
          multi_part_split/2]).

-export ([get_data/2]).

-export ([decode/1,
          media_type_decode/1,
          terms_to_string/1,
          terms_to_atom/1,
          terms_to_tuple/1,
          terms_to_integer/1,
          trim/1,
          tokenize/1]).

-export ([test/0, msg_server/3]).
-author ('csanto@diit.unict.it').

-define (CRLF, "\r\n").


start () ->
  start (nil, []).

%
% The function passed as parameter, as {Module, Func}, has the following
% prototype:
%
%  [Socket, [Method, Url, Headers, Content], Params]
%
start (Function, Params) ->
  start (8080, Function, Params).

start (Port, Function, Params) ->
  {ok, LSock} = gen_tcp:listen(Port, [{packet, 0},
                                      {active, false},
                                      {reuseaddr, true}]),
  Pid = spawn (http_server, main_server, [LSock, Function, Params]),
  {ok, Pid}.


start_sync (Port, Function, Params) ->
  {ok, LSock} = gen_tcp:listen(Port, [{packet, 0},
                                      {active, false},
                                      {reuseaddr, true}]),
  main_server (LSock, Function, Params).



main_server (LSock, Function, Params) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  %process_request (Sock, Function, Params),
  case lists:member (sequential, Params) of
    true -> process_request (Sock, Function, Params);
    _ -> spawn (http_server, process_request, [Sock, Function, Params])
  end,
  main_server (LSock, Function, Params).


process_request (ConnectedSocket, Function, Params) ->
  case catch (get_and_process_data (ConnectedSocket, Function, Params)) of
    {'EXIT', OtherExc} ->
      io:format ("ERROR IN HTTP_SERVER=~p\n", [OtherExc]),
      Closing = true;
    Other -> Closing = Other
  end,
  %%io:format ("Closing ~p~n", [Closing]),
  if
    Closing -> gen_tcp:close (ConnectedSocket);
    true ->
      case lists:member (dont_close, Params) of
        true -> ok;
        _ -> gen_tcp:close (ConnectedSocket), ok
      end
  end.

get_and_process_data (Sock, Function, Params) ->
  {Headers, Body} = get_data (Sock, []),
  process_data (Sock, Headers, Body, Function, Params).

get_data (Sock, Bs) ->
  case gen_tcp:recv (Sock, 0) of
    {ok, B} ->
      %io:format ("A = ~p,~p\n", [Bs, B]),
      Data = Bs ++ B,
      %io:format ("D = ~w\n", [Data]),
      {Hd, Bd, Found} = scan_for_eol (Data),
      if
        Found -> {Header, Body} = {Hd, Bd};
        true -> {Header, Body} = get_data (Sock, Data)
      end;
    {error, closed} ->
      {Header, Body, Found} = scan_for_eol (Bs)
  end,
  {Header, Body}.

scan_for_eol ([], {Header, Body, Found}) ->
  {Header, Body, Found};
scan_for_eol ([ $\r , $\n , $\r , $\n | T], {Header, Body, Found}) ->
  {Header ++ [$\r , $\n], T, true};
scan_for_eol ([ H | T], {Header, Body, Found}) ->
  scan_for_eol (T, {Header ++ [H], Body, false}).

scan_for_eol (X) ->
  scan_for_eol (X, {[], [], false}).

process_data (Socket, Header, Body, Function, Params) ->
  {ok, [request, Request, Headers]} = decode (Header),
  [Method, Url, ProtoVersion] = Request,
  %%io:format ("Hdrs ~p~n", [Headers]),
  %%ConvertedHeaders = remove_key (Headers, "Host"),
  %io:format ("[http_server] Request: ~w -- ~s\n", [Method, Url]),
  if
    Method == 'POST' ->
      Len = list_to_integer(get_key (Headers, "Content-Length")) -
        length (Body),
      if
        Len > 0 -> {ok, BBody} = gen_tcp:recv (Socket, Len);
        true -> BBody = []
      end,
      Content = Body ++ BBody;
    true ->
      Content = Body
  end,
  R = call_proxy_processor (Function,
                            [Socket, [Method, Url, Headers, Content],
                             Params]),
  {Status, RespString, RespHeader, RespBody} = R,
  %%io:format ("[http_server] Reply: ~w -- ~s -- ~w\n", [Method, Url, Status]),
  send_reply (Socket, Status, RespString),
  send_headers (Socket, RespHeader),
  send_body (Socket, RespBody),
  %%io:format ("HDR = ~p~n", [get_key (Headers, "connection")]),
  %%io:format ("HDR = ~p~n", [get_key (Headers, "Connection")]),
  case get_key (Headers, "connection") of
    "close" -> C1 = true;
    _ -> C1 = false
  end,
  case get_key (Headers, "Connection") of
    "close" -> C2 = true;
    _ -> C2 = false
  end, true.
%  C1 or C2 or (Status =/= 200).

call_proxy_processor (Function, Parameters) ->
  if
    Function /= nil ->
      case catch (apply (Function, Parameters)) of
        {'EXIT', Exc} -> io:format ("ERROR IN CALLBACK=~w\n", [Exc]),
                         R = nil;
        X -> R = X
      end;
    true -> R = nil
  end,
  R.

convert_method ('GET') -> 'get';
convert_method ('POST') -> 'post';
convert_method ('HEAD') -> 'head';
convert_method ('CONNECT') -> 'connect';
convert_method (X) -> X.


isuppercase (H) -> (H >= $A) and (H =< $Z).

islowercase ([]) -> true;
islowercase ([H|T]) -> islowercase (H) and islowercase (T);
islowercase (H) when not is_list(H) -> (H >= $a) and (H =< $z).

tolower (H) -> H + 32.

lowcase ([]) -> [];
lowcase ([H | T]) when (H >= $A) and (H =< $Z) -> [tolower (H) | lowcase (T)];
lowcase ([H | T]) ->  [H | lowcase (T)].



get_key__ ([], K) ->
  [];
get_key__ ([{Key, Value} | T], Key) ->
  Value;
get_key__ ([{Key, Value} | T], K) ->
  case lowcase (Key) == K of
    true -> Value;
    _ -> get_key__ (T, K)
  end.

get_key (Pairs, Key) -> get_key__ (Pairs, lowcase (Key)).


remove_key__ ([], KeyToRemove) ->
  [];
remove_key__ ([{Key, Value} | T], KeyToRemove) when Key == KeyToRemove ->
  remove_key__ (T, KeyToRemove);
remove_key__ ([{Key, Value} | T], KeyToRemove) ->
  case lowcase (Key) == KeyToRemove of
    true -> remove_key__ (T, KeyToRemove);
    _ -> [{Key, Value} | remove_key__ (T, KeyToRemove)]
  end.


remove_key (Pairs, Key) -> remove_key__ (Pairs, lowcase (Key)).


send_reply (Socket, Status, RString) ->
  gen_tcp:send (Socket,
                "HTTP/1.1 " ++ integer_to_list (Status) ++ " " ++
                RString ++ ?CRLF).

send_headers (Socket, []) ->
  gen_tcp:send (Socket, ?CRLF);
send_headers (Socket, [{Key, Value} | T]) ->
  if
    is_atom (Key) -> K = atom_to_list (Key);
    true -> K = Key
  end,
  gen_tcp:send (Socket, K ++ ": " ++ Value ++ ?CRLF),
  send_headers (Socket, T).

send_body (Socket, Body) ->
  gen_tcp:send (Socket, Body).

get_body (Sock, Bs) ->
  case gen_tcp:recv (Sock, 0) of
    {ok, B} ->
      get_body (Sock, Bs ++ B);
    {error, closed} ->
      Bs
  end.

get_body (Sock, Bs, Len) when Len > 0 ->
  case gen_tcp:recv (Sock, 0) of
    {ok, B} ->
      Data = Bs ++ B,
      get_body (Sock, Data, Len - length (Data));
    {error, closed} ->
      Bs
  end;

get_body (Sock, Bs, Len)  ->
  Bs.


split_lines (AccList, Acc0, []) ->
  if
    Acc0 =/= [] -> AccL = [lists:reverse (Acc0) | AccList];
    true -> AccL = AccList
  end,
  lists:reverse (AccL);
split_lines (AccList, Acc0, [ H ]) ->
  split_lines (AccList, [H | Acc0], []);
split_lines (AccList, Acc0, [ $\r, $\n | T]) ->
  split_lines ([lists:reverse (Acc0) | AccList], [], T);
split_lines (AccList, Acc0, [ H | T]) ->
  split_lines (AccList, [H | Acc0], T).

split_lines (Lines) -> split_lines ([], [], Lines).


multi_part_split (AccList, Acc0, [], _) ->
  if
    Acc0 =/= [] -> AccL = [lists:reverse (Acc0) | AccList];
    true -> AccL = AccList
  end,
  lists:reverse (AccL);
multi_part_split (AccList, Acc0, [H | T], Boundary) ->
  Index = string:str (H, Boundary),
  if
    Index == 0 ->
      multi_part_split (AccList, [H | Acc0], T, Boundary);
    true ->
      multi_part_split ([lists:reverse (Acc0) | AccList], [], T, Boundary)
  end.
multi_part_split (Data, Boundary) ->
  multi_part_split ([], [], Data, Boundary).

tokenize ([]) -> [{'$end',1}];
tokenize ([H|T]) -> [ {list_to_atom ([H]),1} | tokenize (T) ].


terms_to_string ([]) -> [];
terms_to_string ([H|T]) ->
  [ L | _ ] = atom_to_list (element (1, H)),
  [ L | terms_to_string (T) ].

terms_to_atom (X) ->
  list_to_atom (terms_to_string (X)).

terms_to_tuple (X) ->
  list_to_tuple (terms_to_string (X)).

terms_to_integer (X) ->
  Z = terms_to_string (X),
  list_to_integer (Z).

ltrim ([]) ->
  [];
ltrim ([$ | T]) ->
  ltrim (T);
ltrim ([H | T]) ->
  [H | T].

trim (S) ->
  lists:reverse (ltrim (lists:reverse (ltrim (S)))).

decode (M) ->
  T = tokenize (M),
  http_parser:parse (T).


media_type_decode (M) ->
  T = tokenize (M),
  media_type:parse (T).


display_params ([]) -> ok;
display_params ([{Param, Value} | T]) ->
  io:format ("~s = ~s~n", [Param, Value]),
  display_params (T).


test () ->
  start ({http_server, msg_server}, nil).

msg_server (Socket, [Method, Url, Headers, Content], Params) ->
  io:format ("URL = ~s~n", [Url]),
  display_params (Headers),
  io:format ("Content = ~w~n", [split_lines(Content)]),
  ContentType = get_key (Headers, "Content-Type"),
  [Encoding, Media, Params] = media_type_decode (ContentType),
  {200, "OK", [{"Content-Type", "text/plain"},
               {"Cache-Control", "no-cache"},
               {"Connection", "close"}],
   ""}.

