-module(sl_parser).
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



-file("sl_parser.erl", 97).

yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(acl, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(2, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 yeccpars2(yeccgoto(vcharstring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(term, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(7, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_7_(__Stack),
 yeccpars2(yeccgoto(vcharstringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(elem, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(elem, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(elem, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(16, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 yeccpars2(yeccgoto(elements, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 yeccpars2(yeccgoto(wsp, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_23_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(number, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(39, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(vchar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
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
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(64, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(68, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(upcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(80, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(88, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(92, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(99, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(101, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(lowcase_letter, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(106, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(108, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 112, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(slotterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_110_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(slot, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(slotterm, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(112, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '\\\\', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(113, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(114, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_115_(__Stack),
 yeccpars2(yeccgoto(quotedstring, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(116, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 117, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '$', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 118, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 119, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 120, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 121, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '\\\\', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 122, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [116 | __Ss], [__T | __Stack]);
yeccpars2(116, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_116_(__Stack),
 yeccpars2(yeccgoto(astringterms, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(117, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(121, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(achar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(122, '"', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 123, [122 | __Ss], [__T | __Stack]);
yeccpars2(122, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_123_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(achar, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(124, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_124_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(astringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(125, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_125_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(slotterm, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_126_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(wsp, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(127, '!', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '0', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '1', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '2', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '3', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '4', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '5', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '6', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '7', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '8', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '9', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '?', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'A', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'B', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'C', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'D', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'E', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'F', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'G', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'H', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'I', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'J', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'K', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'L', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'M', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'N', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'O', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 60, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'P', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'Q', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'R', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'S', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'T', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'U', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 66, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'V', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'W', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'X', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'Y', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, 'Z', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '_', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, a, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, b, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, c, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, d, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, e, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, f, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, g, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, h, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 83, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, i, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, j, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, k, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, l, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, m, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, n, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, o, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, p, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, q, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, r, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, s, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, t, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, u, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, v, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 97, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, w, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 98, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, y, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, z, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [127 | __Ss], [__T | __Stack]);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_127_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(elements, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(128, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_128_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(elements, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(129, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(130, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_130_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_131_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(132, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_132_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(vcharstringterms, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(133, ' ', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(134, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(135, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_135_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(136, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_136_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(achar, 112) ->
 116;
yeccgoto(achar, 116) ->
 116;
yeccgoto(acl, 0) ->
 2;
yeccgoto(astringterms, 112) ->
 115;
yeccgoto(astringterms, 116) ->
 124;
yeccgoto(elem, 3) ->
 17;
yeccgoto(elem, 4) ->
 17;
yeccgoto(elem, 127) ->
 17;
yeccgoto(elements, 3) ->
 16;
yeccgoto(elements, 4) ->
 133;
yeccgoto(elements, 127) ->
 128;
yeccgoto(letter, 3) ->
 15;
yeccgoto(letter, 4) ->
 15;
yeccgoto(letter, 7) ->
 15;
yeccgoto(letter, 39) ->
 15;
yeccgoto(letter, 108) ->
 15;
yeccgoto(letter, 112) ->
 15;
yeccgoto(letter, 116) ->
 15;
yeccgoto(letter, 127) ->
 15;
yeccgoto(list, 0) ->
 1;
yeccgoto(list, 3) ->
 14;
yeccgoto(list, 4) ->
 14;
yeccgoto(list, 108) ->
 111;
yeccgoto(list, 127) ->
 14;
yeccgoto(lowcase_letter, 3) ->
 13;
yeccgoto(lowcase_letter, 4) ->
 13;
yeccgoto(lowcase_letter, 7) ->
 13;
yeccgoto(lowcase_letter, 39) ->
 13;
yeccgoto(lowcase_letter, 108) ->
 13;
yeccgoto(lowcase_letter, 112) ->
 13;
yeccgoto(lowcase_letter, 116) ->
 13;
yeccgoto(lowcase_letter, 127) ->
 13;
yeccgoto(number, 3) ->
 12;
yeccgoto(number, 4) ->
 12;
yeccgoto(number, 7) ->
 12;
yeccgoto(number, 39) ->
 12;
yeccgoto(number, 108) ->
 12;
yeccgoto(number, 112) ->
 12;
yeccgoto(number, 116) ->
 12;
yeccgoto(number, 127) ->
 12;
yeccgoto(quotedstring, 112) ->
 114;
yeccgoto(slot, 3) ->
 11;
yeccgoto(slot, 4) ->
 11;
yeccgoto(slot, 127) ->
 11;
yeccgoto(slotterm, 108) ->
 110;
yeccgoto(symbol, 3) ->
 10;
yeccgoto(symbol, 4) ->
 10;
yeccgoto(symbol, 7) ->
 10;
yeccgoto(symbol, 39) ->
 10;
yeccgoto(symbol, 108) ->
 10;
yeccgoto(symbol, 112) ->
 10;
yeccgoto(symbol, 116) ->
 10;
yeccgoto(symbol, 127) ->
 10;
yeccgoto(term, 3) ->
 9;
yeccgoto(term, 4) ->
 9;
yeccgoto(term, 39) ->
 106;
yeccgoto(term, 108) ->
 109;
yeccgoto(term, 127) ->
 9;
yeccgoto(upcase_letter, 3) ->
 8;
yeccgoto(upcase_letter, 4) ->
 8;
yeccgoto(upcase_letter, 7) ->
 8;
yeccgoto(upcase_letter, 39) ->
 8;
yeccgoto(upcase_letter, 108) ->
 8;
yeccgoto(upcase_letter, 112) ->
 8;
yeccgoto(upcase_letter, 116) ->
 8;
yeccgoto(upcase_letter, 127) ->
 8;
yeccgoto(vchar, 3) ->
 7;
yeccgoto(vchar, 4) ->
 7;
yeccgoto(vchar, 7) ->
 7;
yeccgoto(vchar, 39) ->
 7;
yeccgoto(vchar, 108) ->
 7;
yeccgoto(vchar, 112) ->
 113;
yeccgoto(vchar, 116) ->
 113;
yeccgoto(vchar, 127) ->
 7;
yeccgoto(vcharstring, 3) ->
 6;
yeccgoto(vcharstring, 4) ->
 6;
yeccgoto(vcharstring, 39) ->
 6;
yeccgoto(vcharstring, 108) ->
 6;
yeccgoto(vcharstring, 127) ->
 6;
yeccgoto(vcharstringterms, 3) ->
 5;
yeccgoto(vcharstringterms, 4) ->
 5;
yeccgoto(vcharstringterms, 7) ->
 132;
yeccgoto(vcharstringterms, 39) ->
 5;
yeccgoto(vcharstringterms, 108) ->
 5;
yeccgoto(vcharstringterms, 127) ->
 5;
yeccgoto(wsp, 3) ->
 4;
yeccgoto(wsp, 16) ->
 129;
yeccgoto(wsp, 17) ->
 127;
yeccgoto(wsp, 18) ->
 126;
yeccgoto(wsp, 106) ->
 108;
yeccgoto(wsp, 133) ->
 134;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_5_,1}}).
-file("sl_parser.yrl", 84).
yeccpars2_5_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("sl_parser.yrl", 86).
yeccpars2_7_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("sl_parser.yrl", 54).
yeccpars2_17_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("sl_parser.yrl", 0).
yeccpars2_18_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("sl_parser.yrl", 47).
yeccpars2_23_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("sl_parser.yrl", 61).
yeccpars2_110_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { list_to_atom ( __2 ) , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("sl_parser.yrl", 88).
yeccpars2_115_([__1 | __Stack]) ->
 [begin
   http_server : terms_to_string ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_116_,1}}).
-file("sl_parser.yrl", 90).
yeccpars2_116_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("sl_parser.yrl", 98).
yeccpars2_123_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("sl_parser.yrl", 89).
yeccpars2_124_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_125_,1}}).
-file("sl_parser.yrl", 62).
yeccpars2_125_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("sl_parser.yrl", 0).
yeccpars2_126_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("sl_parser.yrl", 55).
yeccpars2_127_([__2,__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_128_,1}}).
-file("sl_parser.yrl", 56).
yeccpars2_128_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 ] ++ __3
  end | __Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("sl_parser.yrl", 49).
yeccpars2_130_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_131_,1}}).
-file("sl_parser.yrl", 51).
yeccpars2_131_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("sl_parser.yrl", 85).
yeccpars2_132_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("sl_parser.yrl", 50).
yeccpars2_135_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_136_,1}}).
-file("sl_parser.yrl", 52).
yeccpars2_136_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].


