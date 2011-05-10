-module(media_type).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/local/lib/erlang/lib/parsetools-1.4/include/yeccpre.hrl", 0).
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("media_type.erl", 97).

yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_2_(__Stack),
 yeccpars2(yeccgoto(string, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(3, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_3_(__Stack),
 yeccpars2(yeccgoto(stringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(4, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(stringterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(6, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(stringterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(stringterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(64, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(68, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(72, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(media_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_75_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(media_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(76, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_76_(__Stack),
 yeccpars2(yeccgoto(params, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(77, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 yeccpars2(yeccgoto(wsp, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_86_(__Stack),
 yeccpars2(yeccgoto(astring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_87_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(param, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(88, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_88_(__Stack),
 yeccpars2(yeccgoto(astringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(91, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(92, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(99, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(106, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(112, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(113, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(114, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(116, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(117, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(118, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [118 | __Ss], [__T | __Stack]);
yeccpars2(118, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_119_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(param, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_120_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(astringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(121, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [121 | __Ss], [__T | __Stack]);
yeccpars2(121, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(122, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_123_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(param, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(124, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(125, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_126_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(param, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_127_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(wsp, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(128, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_128_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(params, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(129, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_129_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(media_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(130, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_130_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(stringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(achar, 81) ->
 88;
yeccgoto(achar, 88) ->
 88;
yeccgoto(achar, 91) ->
 88;
yeccgoto(achar, 122) ->
 88;
yeccgoto(achar, 124) ->
 88;
yeccgoto(astring, 81) ->
 87;
yeccgoto(astring, 91) ->
 118;
yeccgoto(astring, 122) ->
 123;
yeccgoto(astring, 124) ->
 125;
yeccgoto(astringterms, 81) ->
 86;
yeccgoto(astringterms, 88) ->
 120;
yeccgoto(astringterms, 91) ->
 86;
yeccgoto(astringterms, 122) ->
 86;
yeccgoto(astringterms, 124) ->
 86;
yeccgoto(letter, 0) ->
 8;
yeccgoto(letter, 3) ->
 8;
yeccgoto(letter, 72) ->
 8;
yeccgoto(letter, 78) ->
 8;
yeccgoto(letter, 79) ->
 8;
yeccgoto(letter, 81) ->
 85;
yeccgoto(letter, 88) ->
 85;
yeccgoto(letter, 91) ->
 85;
yeccgoto(letter, 122) ->
 85;
yeccgoto(letter, 124) ->
 85;
yeccgoto(lowcase_letter, 0) ->
 7;
yeccgoto(lowcase_letter, 3) ->
 7;
yeccgoto(lowcase_letter, 72) ->
 7;
yeccgoto(lowcase_letter, 78) ->
 7;
yeccgoto(lowcase_letter, 79) ->
 7;
yeccgoto(lowcase_letter, 81) ->
 7;
yeccgoto(lowcase_letter, 88) ->
 7;
yeccgoto(lowcase_letter, 91) ->
 7;
yeccgoto(lowcase_letter, 122) ->
 7;
yeccgoto(lowcase_letter, 124) ->
 7;
yeccgoto(media_type, 0) ->
 6;
yeccgoto(number, 0) ->
 5;
yeccgoto(number, 3) ->
 5;
yeccgoto(number, 72) ->
 5;
yeccgoto(number, 78) ->
 5;
yeccgoto(number, 79) ->
 5;
yeccgoto(number, 81) ->
 84;
yeccgoto(number, 88) ->
 84;
yeccgoto(number, 91) ->
 84;
yeccgoto(number, 122) ->
 84;
yeccgoto(number, 124) ->
 84;
yeccgoto(param, 73) ->
 76;
yeccgoto(param, 74) ->
 76;
yeccgoto(param, 76) ->
 76;
yeccgoto(params, 73) ->
 75;
yeccgoto(params, 74) ->
 129;
yeccgoto(params, 76) ->
 128;
yeccgoto(string, 0) ->
 4;
yeccgoto(string, 72) ->
 73;
yeccgoto(string, 78) ->
 80;
yeccgoto(string, 79) ->
 121;
yeccgoto(stringterm, 0) ->
 3;
yeccgoto(stringterm, 3) ->
 3;
yeccgoto(stringterm, 72) ->
 3;
yeccgoto(stringterm, 78) ->
 3;
yeccgoto(stringterm, 79) ->
 3;
yeccgoto(stringterms, 0) ->
 2;
yeccgoto(stringterms, 3) ->
 130;
yeccgoto(stringterms, 72) ->
 2;
yeccgoto(stringterms, 78) ->
 2;
yeccgoto(stringterms, 79) ->
 2;
yeccgoto(symbol, 81) ->
 83;
yeccgoto(symbol, 88) ->
 83;
yeccgoto(symbol, 91) ->
 83;
yeccgoto(symbol, 122) ->
 83;
yeccgoto(symbol, 124) ->
 83;
yeccgoto(upcase_letter, 0) ->
 1;
yeccgoto(upcase_letter, 3) ->
 1;
yeccgoto(upcase_letter, 72) ->
 1;
yeccgoto(upcase_letter, 78) ->
 1;
yeccgoto(upcase_letter, 79) ->
 1;
yeccgoto(upcase_letter, 81) ->
 1;
yeccgoto(upcase_letter, 88) ->
 1;
yeccgoto(upcase_letter, 91) ->
 1;
yeccgoto(upcase_letter, 122) ->
 1;
yeccgoto(upcase_letter, 124) ->
 1;
yeccgoto(vchar, 81) ->
 82;
yeccgoto(vchar, 88) ->
 82;
yeccgoto(vchar, 91) ->
 82;
yeccgoto(vchar, 122) ->
 82;
yeccgoto(vchar, 124) ->
 82;
yeccgoto(wsp, 73) ->
 74;
yeccgoto(wsp, 77) ->
 127;
yeccgoto(wsp, 78) ->
 79;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_2_,1}}).
-file("media_type.yrl", 60).
yeccpars2_2_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("media_type.yrl", 62).
yeccpars2_3_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("media_type.yrl", 43).
yeccpars2_73_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 , [ ] ]
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("media_type.yrl", 44).
yeccpars2_75_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 , __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("media_type.yrl", 47).
yeccpars2_76_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("media_type.yrl", 0).
yeccpars2_77_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("media_type.yrl", 72).
yeccpars2_86_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("media_type.yrl", 50).
yeccpars2_87_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , http_server : trim ( __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("media_type.yrl", 74).
yeccpars2_88_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("media_type.yrl", 52).
yeccpars2_119_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , http_server : trim ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("media_type.yrl", 73).
yeccpars2_120_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("media_type.yrl", 51).
yeccpars2_123_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __3 , http_server : trim ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("media_type.yrl", 54).
yeccpars2_126_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __3 , http_server : trim ( __6 ) }
  end | __Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("media_type.yrl", 0).
yeccpars2_127_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_128_,1}}).
-file("media_type.yrl", 48).
yeccpars2_128_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("media_type.yrl", 45).
yeccpars2_129_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 , __5 ]
  end | __Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("media_type.yrl", 61).
yeccpars2_130_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].


