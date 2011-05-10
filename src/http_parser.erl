-module(http_parser).
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



-file("http_parser.erl", 97).

yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_3_(__Stack),
 yeccpars2(yeccgoto(string, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(4, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_4_(__Stack),
 yeccpars2(yeccgoto(stringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 yeccpars2(yeccgoto(verb, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(stringterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(stringterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(11, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(http_message, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(http_message, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(14, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 yeccpars2(yeccgoto(http_request, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(stringterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
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
yeccpars2(34, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [34 | __Ss], [__T | __Stack]);
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
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
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
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(79, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(proto, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_83_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(http_request, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(84, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 yeccpars2(yeccgoto(http_params, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_86_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(http_params, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(87, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 123, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(92, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_92_(__Stack),
 yeccpars2(yeccgoto(astring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(93, '\r', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 123, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_94_(__Stack),
 yeccpars2(yeccgoto(astringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(99, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(106, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
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
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(117, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(121, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(122, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(124, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(125, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_125_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(astringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_126_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(http_param, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(127, '\n', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(128, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_128_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(eol, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(129, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_129_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(http_response, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(130, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_131_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(proto_version, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(132, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(133, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(134, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_134_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(version, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(135, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(136, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_136_(__Stack),
 yeccpars2(yeccgoto(wsp, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(137, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_137_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(wsp, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(138, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_138_(__Stack),
 yeccpars2(yeccgoto(integer, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(139, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_139_(__Stack),
 yeccpars2(yeccgoto(numbers, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(140, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(141, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 116, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 123, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(142, '\r', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(143, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_143_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(http_status, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(144, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_144_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(numbers, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(145, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_145_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(stringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(146, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 123, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(147, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_147_(__Stack),
 yeccpars2(yeccgoto(vcharstring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(148, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(url, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(149, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 111, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 113, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 114, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 115, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 123, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 124, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_149_(__Stack),
 yeccpars2(yeccgoto(vcharstringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(150, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(151, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(152, '\r', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(153, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(154, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_154_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(http_header, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(155, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_155_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(vcharstringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(achar, 87) ->
 94;
yeccgoto(achar, 94) ->
 94;
yeccgoto(achar, 141) ->
 94;
yeccgoto(astring, 87) ->
 93;
yeccgoto(astring, 141) ->
 142;
yeccgoto(astringterms, 87) ->
 92;
yeccgoto(astringterms, 94) ->
 125;
yeccgoto(astringterms, 141) ->
 92;
yeccgoto(eol, 93) ->
 126;
yeccgoto(eol, 142) ->
 143;
yeccgoto(eol, 152) ->
 154;
yeccgoto(http_header, 0) ->
 15;
yeccgoto(http_message, 0) ->
 14;
yeccgoto(http_param, 11) ->
 84;
yeccgoto(http_param, 15) ->
 84;
yeccgoto(http_param, 84) ->
 84;
yeccgoto(http_params, 11) ->
 129;
yeccgoto(http_params, 15) ->
 83;
yeccgoto(http_params, 84) ->
 86;
yeccgoto(http_request, 0) ->
 13;
yeccgoto(http_response, 0) ->
 12;
yeccgoto(http_status, 0) ->
 11;
yeccgoto(integer, 135) ->
 140;
yeccgoto(letter, 0) ->
 10;
yeccgoto(letter, 4) ->
 10;
yeccgoto(letter, 11) ->
 10;
yeccgoto(letter, 15) ->
 10;
yeccgoto(letter, 84) ->
 10;
yeccgoto(letter, 87) ->
 91;
yeccgoto(letter, 94) ->
 91;
yeccgoto(letter, 141) ->
 91;
yeccgoto(letter, 146) ->
 91;
yeccgoto(letter, 149) ->
 91;
yeccgoto(lowcase_letter, 0) ->
 9;
yeccgoto(lowcase_letter, 4) ->
 9;
yeccgoto(lowcase_letter, 11) ->
 9;
yeccgoto(lowcase_letter, 15) ->
 9;
yeccgoto(lowcase_letter, 84) ->
 9;
yeccgoto(lowcase_letter, 87) ->
 9;
yeccgoto(lowcase_letter, 94) ->
 9;
yeccgoto(lowcase_letter, 141) ->
 9;
yeccgoto(lowcase_letter, 146) ->
 9;
yeccgoto(lowcase_letter, 149) ->
 9;
yeccgoto(number, 0) ->
 8;
yeccgoto(number, 4) ->
 8;
yeccgoto(number, 11) ->
 8;
yeccgoto(number, 15) ->
 8;
yeccgoto(number, 84) ->
 8;
yeccgoto(number, 87) ->
 90;
yeccgoto(number, 94) ->
 90;
yeccgoto(number, 130) ->
 132;
yeccgoto(number, 133) ->
 134;
yeccgoto(number, 135) ->
 139;
yeccgoto(number, 139) ->
 139;
yeccgoto(number, 141) ->
 90;
yeccgoto(number, 146) ->
 90;
yeccgoto(number, 149) ->
 90;
yeccgoto(numbers, 135) ->
 138;
yeccgoto(numbers, 139) ->
 144;
yeccgoto(proto, 0) ->
 7;
yeccgoto(proto, 151) ->
 7;
yeccgoto(proto_version, 0) ->
 6;
yeccgoto(proto_version, 151) ->
 152;
yeccgoto(string, 0) ->
 5;
yeccgoto(string, 11) ->
 82;
yeccgoto(string, 15) ->
 82;
yeccgoto(string, 84) ->
 82;
yeccgoto(stringterm, 0) ->
 4;
yeccgoto(stringterm, 4) ->
 4;
yeccgoto(stringterm, 11) ->
 4;
yeccgoto(stringterm, 15) ->
 4;
yeccgoto(stringterm, 84) ->
 4;
yeccgoto(stringterms, 0) ->
 3;
yeccgoto(stringterms, 4) ->
 145;
yeccgoto(stringterms, 11) ->
 3;
yeccgoto(stringterms, 15) ->
 3;
yeccgoto(stringterms, 84) ->
 3;
yeccgoto(symbol, 87) ->
 89;
yeccgoto(symbol, 94) ->
 89;
yeccgoto(symbol, 141) ->
 89;
yeccgoto(symbol, 146) ->
 89;
yeccgoto(symbol, 149) ->
 89;
yeccgoto(upcase_letter, 0) ->
 2;
yeccgoto(upcase_letter, 4) ->
 2;
yeccgoto(upcase_letter, 11) ->
 2;
yeccgoto(upcase_letter, 15) ->
 2;
yeccgoto(upcase_letter, 84) ->
 2;
yeccgoto(upcase_letter, 87) ->
 2;
yeccgoto(upcase_letter, 94) ->
 2;
yeccgoto(upcase_letter, 141) ->
 2;
yeccgoto(upcase_letter, 146) ->
 2;
yeccgoto(upcase_letter, 149) ->
 2;
yeccgoto(url, 146) ->
 150;
yeccgoto(vchar, 87) ->
 88;
yeccgoto(vchar, 94) ->
 88;
yeccgoto(vchar, 141) ->
 88;
yeccgoto(vchar, 146) ->
 149;
yeccgoto(vchar, 149) ->
 149;
yeccgoto(vcharstring, 146) ->
 148;
yeccgoto(vcharstringterms, 146) ->
 147;
yeccgoto(vcharstringterms, 149) ->
 155;
yeccgoto(verb, 0) ->
 1;
yeccgoto(version, 130) ->
 131;
yeccgoto(wsp, 1) ->
 146;
yeccgoto(wsp, 6) ->
 135;
yeccgoto(wsp, 136) ->
 137;
yeccgoto(wsp, 140) ->
 141;
yeccgoto(wsp, 150) ->
 151;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_3_,1}}).
-file("http_parser.yrl", 86).
yeccpars2_3_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("http_parser.yrl", 88).
yeccpars2_4_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("http_parser.yrl", 61).
yeccpars2_5_([__1 | __Stack]) ->
 [begin
   list_to_atom ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("http_parser.yrl", 49).
yeccpars2_15_([__1 | __Stack]) ->
 [begin
   [ request , __1 , nil ]
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("http_parser.yrl", 69).
yeccpars2_81_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   http_server : terms_to_atom ( [ __1 , __2 , __3 , __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("http_parser.yrl", 50).
yeccpars2_83_([__2,__1 | __Stack]) ->
 [begin
   [ request , __1 , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("http_parser.yrl", 77).
yeccpars2_84_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("http_parser.yrl", 78).
yeccpars2_86_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("http_parser.yrl", 98).
yeccpars2_92_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("http_parser.yrl", 100).
yeccpars2_94_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("http_parser.yrl", 99).
yeccpars2_125_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("http_parser.yrl", 80).
yeccpars2_126_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , http_server : trim ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_128_,1}}).
-file("http_parser.yrl", 0).
yeccpars2_128_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_129_,1}}).
-file("http_parser.yrl", 52).
yeccpars2_129_([__2,__1 | __Stack]) ->
 [begin
   [ response , __1 , __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("http_parser.yrl", 66).
yeccpars2_131_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_134_,1}}).
-file("http_parser.yrl", 72).
yeccpars2_134_([__3,__2,__1 | __Stack]) ->
 [begin
   { http_server : terms_to_integer ( [ __1 ] ) , http_server : terms_to_integer ( [ __3 ] ) }
  end | __Stack].

-compile({inline,{yeccpars2_136_,1}}).
-file("http_parser.yrl", 0).
yeccpars2_136_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("http_parser.yrl", 0).
yeccpars2_137_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_138_,1}}).
-file("http_parser.yrl", 82).
yeccpars2_138_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_integer ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("http_parser.yrl", 84).
yeccpars2_139_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("http_parser.yrl", 55).
yeccpars2_143_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __3 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_144_,1}}).
-file("http_parser.yrl", 83).
yeccpars2_144_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("http_parser.yrl", 87).
yeccpars2_145_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("http_parser.yrl", 94).
yeccpars2_147_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("http_parser.yrl", 96).
yeccpars2_149_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("http_parser.yrl", 59).
yeccpars2_154_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 , __3 , __5 ]
  end | __Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("http_parser.yrl", 95).
yeccpars2_155_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].


