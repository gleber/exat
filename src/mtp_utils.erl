-module(mtp_utils).

-export([replace_newlines/2, skip_to_empty_line/1]).

%%
%%

skip_to_empty_line([]) -> [];
skip_to_empty_line([[] | T]) -> T;
skip_to_empty_line([_ | T]) -> skip_to_empty_line(T).

%%
%%

replace_newlines(Acc, []) -> lists:reverse(Acc);
replace_newlines(Acc, [$\r | T]) ->
    replace_newlines([$\s | Acc], T);
replace_newlines(Acc, [$\n | T]) ->
    replace_newlines([$\s | Acc], T);
replace_newlines(Acc, [H | T]) ->
    replace_newlines([H | Acc], T).

%%
%%
