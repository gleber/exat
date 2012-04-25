%%
%%ontology.erl
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
-module(ontology).

%%====================================================================
%% Include files
%%====================================================================
-include("ontology.hrl").

%%====================================================================
%% External exports
%%====================================================================
-export([compile/2, compile/4, sl_decode_term/2,
         sl_encode_term/2]).

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Function: compile/2
%% Description: Compiles an ontology file
%%====================================================================
compile(PathName, Options) ->
    FileName = filename:basename(PathName),
    DirName = filename:dirname(PathName),
    compile(FileName, DirName, DirName, Options).

%%====================================================================
%% Function: compile/4
%% Description: Compiles an ontology file
%%====================================================================
compile(FileName, SrcPath, DestPath, Options) ->
    {ok, AbstractErlangForm} = epp:parse_file(SrcPath ++
                                                  "/" ++ FileName ++ ".onto",
                                              "", []),
    %%io:format ("~w~n", [AbstractErlangForm]),
    {ok, Classes} = compile_lines([], list_to_atom(FileName), AbstractErlangForm),
    %%io:format ("~p~n", [Classes]),
    NewClasses = resolve_inheritance(Classes),
    %%io:format ("~p~n", [NewClasses]),
    generate_include(DestPath, FileName, NewClasses, lists:member(include, Options)),
    %%
    generate_erlang(DestPath, FileName, Classes, NewClasses, lists:member(source, Options)),
    %%
    generate_sl_codec(DestPath, FileName, NewClasses, lists:member(sl_codec, Options)),
    %%
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%
%%
%% FUNCTIONS FOR THE GENERATION OF THE INCLUDE FILE
%%
%%
%%====================================================================
%% Func: generate_include/4
%%====================================================================
generate_include(_, _, _, false) -> ok;
generate_include(DestPath, FileName, Classes, _) ->
    IncludeLines = generate_include_file(Classes),
    {ok, IncludeFile} = file:open(DestPath ++
                                      "/" ++ FileName ++ ".hrl",
                                  [write]),
    io:format(IncludeFile, "~s", [IncludeLines]),
    file:close(IncludeFile),
    ok.

%%====================================================================
%% Func: compile_lines/3
%% Description: compile the lines of the ontology, given its erlang
%%              abstract form
%% Returns: {ok,  [#ontology_class]} |
%%          {error, Reason}
%%====================================================================
compile_lines(Accumulator, _, []) ->
    {ok, lists:flatten(lists:reverse(Accumulator))};
compile_lines(Accumulator, OntoName,
              [{function, _, class, _, Clauses} | Tail]) ->
    compile_lines([compile_clauses([], Clauses)
                   | Accumulator],
                  OntoName, Tail);
compile_lines(Accumulator, OntoName,
              [{attribute, _, ontology, OntoName} | Tail]) ->
    compile_lines(Accumulator, OntoName, Tail);
compile_lines(_Accumulator, _OntoName,
              [{attribute, Line, ontology, _} | _Tail]) ->
    {error,
     {"ontology name does not match with filename "
      "in line",
      Line}};
compile_lines(Accumulator, OntoName,
              [{attribute, _, file, _} | Tail]) ->
    compile_lines(Accumulator, OntoName, Tail);
compile_lines(Accumulator, OntoName,
              [{eof, _} | Tail]) ->
    compile_lines(Accumulator, OntoName, Tail);
compile_lines(_Accumulator, _,
              [{_, Line, _, _} | _Tail]) ->
    {error, {"syntax error in line", Line}}.

%%====================================================================
%% Func: compile_clauses/2
%% Description: compile function clauses (classes) of the ontology,
%%              given its erlang abstract form
%% Returns: [#ontology_class]
%%====================================================================
compile_clauses(Acc, []) -> lists:reverse(Acc);
compile_clauses(Acc, [H | T]) ->
    compile_clauses([compile_clause(H) | Acc], T).

%%====================================================================
%% Func: compile_clause/1
%% Description: compile a single function clause (classes) of the ontology,
%%              given its erlang abstract form
%% Returns: #ontology_class
%%====================================================================
compile_clause({clause, _LineNum, [{atom, _, ClassName}],
                [], [{tuple, _, ClassDef}]}) ->
    #ontology_class{name = ClassName, superclass = nil,
                    properties =
                        compile_properties([], ClassName, ClassDef)};
compile_clause({clause, _LineNum, [{atom, _, ClassName}],
                [],
                [{call, _, {atom, _, is_a},
                  [{atom, _, SuperClass}]}]}) ->
    #ontology_class{name = ClassName,
                    superclass = SuperClass, properties = []};
compile_clause({clause, _LineNum, [{atom, _, ClassName}],
                [],
                [{call, _, {atom, _, is_a}, [{atom, _, SuperClass}]},
                 {tuple, _, ClassDef}]}) ->
    #ontology_class{name = ClassName,
                    superclass = SuperClass,
                    properties =
                        compile_properties([], ClassName, ClassDef)}.

%%====================================================================
%% Func: compile_properties/3
%% Description: compile the properties of an ontology class,
%%              given the erlang abstract form
%% Returns: [#ontology_property]
%%====================================================================
compile_properties(Acc, _, []) -> lists:reverse(Acc);
compile_properties(Acc, ClassName, [H | T]) ->
    compile_properties([compile_property(ClassName, H)
                        | Acc],
                       ClassName, T).

%%====================================================================
%% Func: compile_properties/2
%% Description: compile a single property of an ontology class,
%%              given the erlang abstract form
%% Returns: #ontology_property
%%====================================================================
compile_property(_ClassName,
                 {match, _, {atom, _, FieldName}, FieldDef}) ->
    L = cons_to_erl_list(FieldDef),
    %%io:format ("~p~n", [L]),
    [FieldType, FieldRequirement, Default | _] = L,
    #ontology_property{name = FieldName, type = FieldType,
                       requirement = FieldRequirement,
                       is_primitive = is_primitive(FieldType),
                       is_digit = is_digit(FieldName), default = Default}.

%%====================================================================
%% Func: cons_to_erl_list/1
%% Description: transforms a "cons" abstract erlang construct to a list
%% Returns: [term()]
%%====================================================================
cons_to_erl_list({cons, _Line, OP1, OP2}) ->
    [cons_decode(OP1) | cons_to_erl_list(OP2)];
cons_to_erl_list(X) -> [cons_decode(X)].

%%====================================================================
%% Func: cons_decode/1
%% Description: decodes a single abstract erlang term
%% Returns: term()
%%====================================================================
cons_decode({atom, _, nodefault}) -> ?NO_DEFAULT;
cons_decode({atom, _, Option}) -> Option;
cons_decode({nil, _}) -> nil;
cons_decode({call, _, {atom, _, set_of},
             [{atom, _, Type}]}) ->
    {set_of, Type};
cons_decode({call, _, {atom, _, sequence_of},
             [{atom, _, Type}]}) ->
    {sequence_of, Type};
cons_decode({call, _, {atom, _, default},
             [{atom, _, Value}]}) ->
    Value.%cons_decode ({match,_, {atom, _, cardinality},
%%             {tuple,_,[{_, _, Low},{_, _, High}]}}) ->
%% {cardinality, Low, High}.

%%====================================================================
%% Func: is_primitive/1
%% Description: checks if a type is primitive
%% Returns: true | false
%%====================================================================
is_primitive(string) -> true;
is_primitive(number) -> true;
is_primitive(integer) -> true;
is_primitive(boolean) -> true;
is_primitive(any) -> true;
is_primitive({sequence_of, X}) -> is_primitive(X);
is_primitive({set_of, X}) -> is_primitive(X);
is_primitive(_) -> false.

%%====================================================================
%% Func: is_digit/1
%% Description: checks if a slot name is a digit
%% Returns: true | false
%%====================================================================
is_digit('0') -> true;
is_digit('1') -> true;
is_digit('2') -> true;
is_digit('3') -> true;
is_digit('4') -> true;
is_digit('5') -> true;
is_digit('6') -> true;
is_digit('7') -> true;
is_digit('8') -> true;
is_digit('9') -> true;
is_digit(_) -> false.

%%====================================================================
%% Func: resolve_inheritance/1
%% Description: resolves the inheritances in the list of #ontology_class
%% Returns: [#ontology_class]
%%====================================================================
resolve_inheritance(Classes) ->
    case resolve_inheritance([], Classes, Classes) of
        {false, NewClassList} ->
            resolve_inheritance(NewClassList);
        {true, NewClassList} -> NewClassList
    end.

%%====================================================================
%% Func: resolve_inheritance/3
%% Description: resolves the inheritances in the list of #ontology_class
%% Returns: {Solved, [#ontology_class]}
%%====================================================================
resolve_inheritance(Acc, _, []) ->
    {true, lists:reverse(Acc)};
resolve_inheritance(Acc, Classes,
                    [Class = #ontology_class{superclass = nil} | T]) ->
    resolve_inheritance([Class | Acc], Classes, T);
resolve_inheritance(Acc, Classes, [Class | T]) ->
    SuperClass = get_class(Class#ontology_class.superclass,
                           Classes),
    NewClass = Class#ontology_class{properties =
                                        lists:foldl(fun (X, A) ->
                                                            override_property([],
                                                                              A,
                                                                              X)
                                                    end,
                                                    SuperClass#ontology_class.properties,
                                                    Class#ontology_class.properties),
                                    superclass = nil},
    {false, lists:reverse([NewClass | Acc]) ++ T}.

override_property(Acc, [], nil) -> lists:reverse(Acc);
override_property(Acc, [],
                  NewProperty = #ontology_property{}) ->
    lists:reverse([NewProperty | Acc]);
override_property(Acc,
                  [#ontology_property{name = N} | T],
                  NewProperty = #ontology_property{name = N}) ->
    override_property([NewProperty | Acc], T, nil);
override_property(Acc, [P | T], NewProperty) ->
    override_property([P | Acc], T, NewProperty).

%%====================================================================
%% Func: get_class/2
%% Description: Searches for a class in the list
%% Returns: #ontology_class
%%====================================================================
get_class(_ClassName, []) -> nil;
get_class(ClassName,
          [Class = #ontology_class{name = ClassName} | _T]) ->
    Class;
get_class(ClassName, [_ | T]) ->
    get_class(ClassName, T).

%%====================================================================
%% Func: generate_hierarchy_tree/3
%% Description: generates the tree of hierarchies
%% Returns: [{classname, [classname]}]
%%====================================================================
generate_hierarchy_tree(Acc, [], _) ->
    lists:reverse(Acc);
generate_hierarchy_tree(Acc, [Class | T], Classes) ->
    Item = {Class#ontology_class.name,
            ancestors_list([], Class#ontology_class.superclass,
                           Classes)},
    generate_hierarchy_tree([Item | Acc], T, Classes).

ancestors_list(Acc, nil, _Classes) -> lists:reverse(Acc);
ancestors_list(Acc, X, Classes) ->
    C = get_class(X, Classes),
    ancestors_list([X | Acc], C#ontology_class.superclass,
                   Classes).

reverse_hierarchy_tree(Acc, [], _) ->
    lists:reverse(Acc);
reverse_hierarchy_tree(Acc, [{Father, _} | T],
                       Classes) ->
    Item = {Father, child_list(Father, Classes)},
    reverse_hierarchy_tree([Item | Acc], T, Classes).

child_list(Father, Classes) ->
    [C
     || {C, Ancestors} <- Classes,
        lists:member(Father, Ancestors)].

%%====================================================================
%% Func: generate_include_file/1
%% Description: generates the include file from a list of #ontology_class
%% Returns: [string()]
%%====================================================================
generate_include_file(Classes) ->
    generate_include_file([], Classes).

generate_include_file(Acc, []) ->
    lists:flatten(lists:reverse(Acc));
generate_include_file(Acc, [Class | T]) ->
    Head = io_lib:format("-record('~s',{~n",
                         [Class#ontology_class.name]),
    Properties = generate_include_lines([],
                                        Class#ontology_class.properties),
    Line = lists:flatten([Head, Properties, "\n"]),
    generate_include_file([Line | Acc], T).

%%====================================================================
%% Func: generate_include_lines/1
%% Description: generates the lines of properties for an include file
%% Returns: [string()]
%%====================================================================
generate_include_lines(Acc, []) ->
    Line = io_lib:format("}).~n", []),
    lists:reverse([Line | Acc]);
generate_include_lines(Acc,
                       [Property = #ontology_property{default =
                                                          ?NO_DEFAULT}]) ->
    Line = io_lib:format("  '~s'",
                         [Property#ontology_property.name]),
    generate_include_lines([Line | Acc], []);
generate_include_lines(Acc,
                       [Property = #ontology_property{default = ?NO_DEFAULT}
                        | T]) ->
    Line = io_lib:format("  '~s',~n",
                         [Property#ontology_property.name]),
    generate_include_lines([Line | Acc], T);
generate_include_lines(Acc, [Property]) ->
    Line = io_lib:format("  '~s' = '~s'",
                         [Property#ontology_property.name,
                          Property#ontology_property.default]),
    generate_include_lines([Line | Acc], []);
generate_include_lines(Acc, [Property | T]) ->
    Line = io_lib:format("  '~s' = '~s',~n",
                         [Property#ontology_property.name,
                          Property#ontology_property.default]),
    generate_include_lines([Line | Acc], T).

%%====================================================================
%%
%%
%% FUNCTIONS FOR THE GENERATION OF THE ERLANG FILE
%%
%%
%%====================================================================
%%====================================================================
%% Func: generate_erlang/5
%%====================================================================
generate_erlang(_, _, _, _, false) -> ok;
generate_erlang(DestPath, FileName, Classes, NewClasses,
                _) ->
    IsAHierarchy = generate_hierarchy_tree([], Classes,
                                           Classes),
    %%io:format ("~p~n", [IsAHierarchy]),
    FatherOfHierarchy = reverse_hierarchy_tree([],
                                               IsAHierarchy, IsAHierarchy),
    %%io:format ("~p~n", [FatherOfHierarchy]),
    IsClassLines = generate_is_class([], IsAHierarchy),
    IsALines = generate_is_a([], IsAHierarchy),
    {CastClasses, CastLines} = generate_cast({[], []},
                                             FatherOfHierarchy, NewClasses),
    ChildOfLines = generate_childof([], FatherOfHierarchy),
    %%
    {ok, ErlangFile} = file:open(DestPath ++
                                     "/" ++ FileName ++ ".erl",
                                 [write]),
    io:format(ErlangFile, "-module (~s).~n", [FileName]),
    io:format(ErlangFile, "-include (\"~s.hrl\").~n",
              [FileName]),
    io:format(ErlangFile,
              "-export ([is_class/1, is_a/2, ~s childof/1])."
              "~n~n",
              [lists:flatten([io_lib:format("'~s'/1,", [X])
                              || X <- CastClasses])]),
    io:format(ErlangFile, "~s", [IsClassLines]),
    io:format(ErlangFile, "~s", [IsALines]),
    io:format(ErlangFile, "~s", [ChildOfLines]),
    io:format(ErlangFile, "~s", [CastLines]),
    file:close(ErlangFile),
    ok.

%%====================================================================
%% Func: generate_childof/2
%% Description: generates the lines for 'childof' functions
%% Returns: [string()]
%%====================================================================
generate_childof(Acc, []) ->
    lists:flatten(lists:reverse(["childof (_) -> exit (undef_class).\n\n"
                                 | Acc]));
generate_childof(Acc,
                 [{FatherClassName, Children} | T]) ->
    Line =
        lists:flatten(io_lib:format("childof ('~s') -> ~p;\n",
                                    [FatherClassName, Children])),
    generate_childof([Line | Acc], T).

%%====================================================================
%% Func: generate_is_a/2
%% Description: generates the lines for 'is_a' functions
%% Returns: [string()]
%%====================================================================
generate_is_a(Acc, []) ->
    lists:flatten(lists:reverse(["is_a (_,_) -> false.\n\n"
                                 | Acc]));
generate_is_a(Acc, [{_ClassName, []} | T]) ->
    generate_is_a(Acc, T);
generate_is_a(Acc, [{ClassName, Ancestors} | T]) ->
    Line =
        [lists:flatten(io_lib:format("is_a ('~s','~s') -> true;\n",
                                     [ClassName, Ancestor]))
         || Ancestor <- Ancestors],
    generate_is_a([Line | Acc], T).

%%====================================================================
%% Func: generate_is_class/1
%% Description: generates the lines for 'is_class' functions
%% Returns: boolean
%%====================================================================
generate_is_class(Acc, []) ->
    lists:flatten(lists:reverse(["is_class (_) -> false.\n\n"
                                 | Acc]));
generate_is_class(Acc, [{ClassName, _} | T]) ->
    Line =
        lists:flatten(io_lib:format("is_class ('~s') -> true;\n",
                                    [ClassName])),
    generate_is_class([Line | Acc], T).

%%====================================================================
%% Func: generate_cast/2
%% Description: generates the lines for cast functions
%% Returns: [string()]
%%====================================================================
generate_cast({Acc1, Acc2}, [], _) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
generate_cast({Acc1, Acc2}, [{_ClassName, []} | T],
              ResolvedClasses) ->
    generate_cast({Acc1, Acc2}, T, ResolvedClasses);
generate_cast({Acc1, Acc2}, [{ClassName, Children} | T],
              ResolvedClasses) ->
    Lines = lists:flatten([generate_cast_1(V1, ClassName,
                                           ResolvedClasses)
                           || V1 <- Children]),
    [CR, CR, _ | ReversedList] = lists:reverse(Lines),
    %%replace last semicolon with a dot to end the clause
    NewLines = lists:reverse([CR, CR, $. | ReversedList]),
    generate_cast({[ClassName | Acc1], [NewLines | Acc2]},
                  T, ResolvedClasses).

generate_cast_1(X, ClassName, ResolvedClasses) ->
    DestinationClass = get_class(ClassName,
                                 ResolvedClasses),
    SourceClass = get_class(X, ResolvedClasses),
    generate_translation_lines(SourceClass,
                               DestinationClass).

generate_translation_lines(SourceClass, DestinationClass) ->
    Lines =
        [lists:flatten(io_lib:format("    '~s' = X#'~s'.'~s'",
                                     [X#ontology_property.name,
                                      SourceClass#ontology_class.name,
                                      X#ontology_property.name]))
         || X <- DestinationClass#ontology_class.properties],
    %%io:format ("~p~n", [Lines]),
    XLines = lists:foldl(fun (X, Sum) ->
                                 lists:concat([Sum, ",\n", X])
                         end,
                         "", Lines),
    %%io:format ("~p~n", [XLines]),
    [_, _ | YLines] = XLines,
    Head =
        lists:flatten(io_lib:format("'~s' (X = #'~s'{}) ->\n  #'~s'{\n",
                                    [DestinationClass#ontology_class.name,
                                     SourceClass#ontology_class.name,
                                     DestinationClass#ontology_class.name])),
    lists:concat([Head, lists:flatten(YLines), "};\n\n"]).

%%====================================================================
%%
%%
%% FUNCTIONS FOR THE GENERATION OF THE SL CODEC FILE
%%
%%
%%====================================================================
%%====================================================================
%% Func: generate_sl_codec/4
%%====================================================================
generate_sl_codec(_, _, _, false) -> ok;
generate_sl_codec(DestPath, FileName, NewClasses, _) ->
    EncoderLines = generate_sl_encoder_file(NewClasses),
    DecoderLines = generate_sl_decoder_file(NewClasses),
    CodecName = FileName ++ "_sl_codec",
    {ok, ConversionFile} = file:open(DestPath ++
                                         "/" ++ CodecName ++ ".erl",
                                     [write]),
    io:format(ConversionFile, "-module (~s).~n",
              [CodecName]),
    io:format(ConversionFile, "-include (\"acl.hrl\").~n",
              []),
    io:format(ConversionFile, "-include (\"~s.hrl\").~n",
              [FileName]),
    io:format(ConversionFile,
              "-export ([encode/1, decode/1]).~n~n", []),
    io:format(ConversionFile, "~s", [EncoderLines]),
    io:format(ConversionFile, "~s", [DecoderLines]),
    file:close(ConversionFile),
    ok.

%%====================================================================
%% Func: generate_sl_encoder_file/1
%% Description: generates the SL encode file from a list of #ontology_class
%% Returns: [string()]
%%====================================================================
generate_sl_encoder_file(Classes) ->
    generate_sl_encoder_file([], Classes).

generate_sl_encoder_file(Acc, []) ->
    lists:flatten(lists:reverse([["encode(X) when is_list (X) -> [encode "
                                  "(Y) || Y <- X];\n\n",
                                  "encode(nil) -> nil;\n\n",
                                  "encode(X) -> exit ({ontology_error,X}).\n\n"]
                                 | Acc]));
generate_sl_encoder_file(Acc, [Class | T]) ->
    Head =
        io_lib:format("encode(X) when is_record (X,'~s') ->~n "
                      " [<<\"~s\">>",
                      [Class#ontology_class.name, Class#ontology_class.name]),
    Properties = generate_sl_encoder_lines([],
                                           Class#ontology_class.name,
                                           Class#ontology_class.properties),
    Line = lists:flatten([Head, Properties, "];\n\n"]),
    generate_sl_encoder_file([Line | Acc], T).

%%====================================================================
%% Func: generate_sl_encoder_lines/3
%% Description: generates the lines of the encoder
%% Returns: [string()]
%%====================================================================
generate_sl_encoder_lines(Acc, _, []) ->
    lists:reverse(Acc);
generate_sl_encoder_lines(Acc, ClassName, [Property]) ->
    Line = io_lib:format(",~s",
                         [field_sl_encode(ClassName,
                                          Property#ontology_property.name,
                                          Property#ontology_property.type,
                                          Property#ontology_property.is_primitive,
                                          Property#ontology_property.is_digit)]),
    generate_sl_encoder_lines([Line | Acc], ClassName, []);
generate_sl_encoder_lines(Acc, ClassName,
                          [Property | T]) ->
    Line = io_lib:format(",~s",
                         [field_sl_encode(ClassName,
                                          Property#ontology_property.name,
                                          Property#ontology_property.type,
                                          Property#ontology_property.is_primitive,
                                          Property#ontology_property.is_digit)]),
    generate_sl_encoder_lines([Line | Acc], ClassName, T).

%%====================================================================
%% Func: field_sl_encode/5
%% Description: generates the encoder for a slot/field
%% Returns: string()
%%====================================================================
%% field_sl_encode (ClassName, FieldName, Type, IsPrimitive, IsDigit)
field_sl_encode(ClassName, FieldName, {sequence_of, _}, false, false) ->
    lists:flatten(io_lib:format("{'~s', [<<\"sequence\">> | encode (X#'~s'.'~s')]}",
                                [FieldName, ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, {sequence_of, _}, false, true) ->
    lists:flatten(io_lib:format("[<<\"sequence\">> | encode (X#'~s'.'~s')]",
                                [ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, {sequence_of, _}, true, false) ->
    lists:flatten(io_lib:format("{'~s', [<<\"sequence\">> | X#'~s'.'~s']}",
                                [FieldName, ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, {sequence_of, _}, true, true) ->
    lists:flatten(io_lib:format("[<<\"sequence\">> | X#'~s'.'~s']",
                                [ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, {set_of, _}, false, _) ->
    lists:flatten(io_lib:format("{'~s', [<<\"set\">> | encode (X#'~s'.'~s') ]}",
                                [FieldName, ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, {set_of, _}, true, _) ->
    lists:flatten(io_lib:format("{'~s',[<<\"set\">> | X#'~s'.'~s' ]}",
                                [FieldName, ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, _, false, false) ->
    lists:flatten(io_lib:format("{'~s', encode (X#'~s'.'~s')}",
                                [FieldName, ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, FieldType, true, false) ->
    lists:flatten(io_lib:format("{'~s', ontology:sl_encode_term (X#'~s'.'~s', ~s)}",
                                [FieldName, ClassName, FieldName, FieldType]));
field_sl_encode(ClassName, FieldName, _, false, true) ->
    lists:flatten(io_lib:format("encode (X#'~s'.'~s')",
                                [ClassName, FieldName]));
field_sl_encode(ClassName, FieldName, FieldType, true, true) ->
    lists:flatten(io_lib:format("ontology:sl_encode_term (X#'~s'.'~s', ~s)",
                                [ClassName, FieldName, FieldType])).

%%====================================================================
%% Func: generate_sl_decoder_file/1
%% Description: generates the SL encode file from a list of #ontology_class
%% Returns: [string()]
%%====================================================================
generate_sl_decoder_file(Classes) ->
    generate_sl_decoder_file([], Classes).

generate_sl_decoder_file(Acc, []) ->
    lists:flatten(lists:reverse([["decode(nil) -> nil;\n\n",
                                  "decode(X) when is_list(X) -> \n",
                                  "    [decode (Y) || Y <- X];\n",
                                  "decode(X) -> X.\n", 
                                  "set_of ([ <<\"set\">> | L]) -> decode (L).\n",
                                  "sequence_of ([ <<\"sequence\">> | L]) -> "
                                  "decode (L).\n",
                                  "\n"]
                                 | Acc]));
generate_sl_decoder_file(Acc, [Class | T]) ->
    %%   PropertyList =
    %%     [if
    %%        X#ontology_property.requirement == mandatory ->
    %%          {'M', X};
    %%        true ->
    %%          {0, X}
    %%      end || X <- Class#ontology_class.properties],
    %%   OptionalPropertyLists = build_optional ([], PropertyList),
    %%   io:format ("~w~n", [OptionalPropertyLists]),
    OptionalPropertyLists =
        [Class#ontology_class.properties],
    Line = [generate_sl_decoder_clause(Class, X)
            || X <- OptionalPropertyLists],
    generate_sl_decoder_file([Line | Acc], T).

generate_sl_decoder_clause(Class, Properties) ->
    Head = io_lib:format("decode([<<\"~s\">> | T]) ->\n",
                         [Class#ontology_class.name]),
    Lines = generate_sl_decoder_lines([],
                                      Class#ontology_class.name, Properties),
    Line = lists:flatten([Head,
                          io_lib:format("  #'~s'{",
                                        [Class#ontology_class.name]),
                          Lines, "};\n\n"]),
    Line.

%% increment ([]) -> throw (eof);
%% increment ([{'M',X} | T]) -> [{'M',X}] ++ increment (T);
%% increment ([{0,X} | T]) -> [{1,X} | T];
%% increment ([{1,X} | T]) -> [{0,X}] ++ increment (T).

%% build_optional (Acc, PropertyList) ->
%%   FilteredValues = lists:filter (
%%                      fun ({0, Data}) -> false;
%%                          ({_,Data}) -> true end, PropertyList),
%%   FinalValues = lists:map (fun ({_,Data}) -> Data end, FilteredValues),
%%   case catch (increment (PropertyList)) of
%%     eof -> lists:reverse ([FinalValues | Acc]);
%%     NewPropertyList -> build_optional ([FinalValues | Acc], NewPropertyList)
%%   end.

%%====================================================================
%% Func: generate_decoder_lines/3
%% Description: generates the lines of the decoder
%% Returns: [string()]
%%====================================================================
generate_sl_decoder_lines(Acc, _, []) ->
    lists:reverse(Acc);
generate_sl_decoder_lines(Acc, ClassName, [Property]) ->
    Line = io_lib:format("~s",
                         [sl_decode_term(ClassName,
                                         Property#ontology_property.name,
                                         Property#ontology_property.type,
                                         Property#ontology_property.is_primitive,
                                         Property#ontology_property.is_digit)]),
    generate_sl_decoder_lines([Line | Acc], ClassName, []);
generate_sl_decoder_lines(Acc, ClassName,
                          [Property | T]) ->
    Line = io_lib:format("~s,",
                         [sl_decode_term(ClassName,
                                         Property#ontology_property.name,
                                         Property#ontology_property.type,
                                         Property#ontology_property.is_primitive,
                                         Property#ontology_property.is_digit)]),
    generate_sl_decoder_lines([Line | Acc], ClassName, T).

sl_decode_term(_ClassName, FieldName, {set_of, _}, false,
               false) ->
    lists:flatten(io_lib:format("'~s' = decode (set_of (sl:get_slot ('~s', "
                                "T)))",
                                [FieldName, FieldName]));
sl_decode_term(_ClassName, FieldName, {set_of, _}, true,
               false) ->
    lists:flatten(io_lib:format("'~s' = set_of (sl:get_slot ('~s', T))",
                                [FieldName, FieldName]));
sl_decode_term(_ClassName, FieldName, {sequence_of, _},
               false, false) ->
    lists:flatten(io_lib:format("'~s' = decode (sequence_of (sl:get_slot "
                                "('~s', T)))",
                                [FieldName, FieldName]));
sl_decode_term(_ClassName, FieldName, {sequence_of, _},
               true, false) ->
    lists:flatten(io_lib:format("'~s' = sequence_of (sl:get_slot ('~s', "
                                "T))",
                                [FieldName, FieldName]));
sl_decode_term(_ClassName, FieldName, _, false, false) ->
    lists:flatten(io_lib:format("'~s' = decode (sl:get_slot ('~s', T))",
                                [FieldName, FieldName]));
sl_decode_term(_ClassName, FieldName, FieldType, true,
               false) ->
    lists:flatten(io_lib:format("'~s' = ontology:sl_decode_term (sl:get_slot "
                                "('~s', T), ~s)",
                                [FieldName, FieldName, FieldType]));
sl_decode_term(_ClassName, FieldName, _, false, true) ->
    lists:flatten(io_lib:format("'~s' = decode (lists:nth (~w, T))",
                                [FieldName, digit_of(FieldName) + 1]));
sl_decode_term(_ClassName, FieldName, FieldType, true,
               true) ->
    lists:flatten(io_lib:format("'~s' = ontology:sl_decode_term (lists:nth "
                                "(~w,T), ~w)",
                                [FieldName, digit_of(FieldName) + 1,
                                 FieldType])).

digit_of('0') -> 0;
digit_of('1') -> 1;
digit_of('2') -> 2;
digit_of('3') -> 3;
digit_of('4') -> 4;
digit_of('5') -> 5;
digit_of('6') -> 6;
digit_of('7') -> 7;
digit_of('8') -> 8;
digit_of('9') -> 9.

%%====================================================================
%% Func: sl_encode_term/2
%% Description: converts a generic term into a string
%% Returns: string() | nil
%%====================================================================
sl_encode_term(nil, _) -> nil;
sl_encode_term(X, integer) -> list_to_binary(integer_to_list(X));
sl_encode_term(X, float) -> list_to_binary(float_to_list(X));
sl_encode_term(X, _) -> X.

%%====================================================================
%% Func: sl_decode_term/2
%% Description: converts a string into a generic term
%% Returns: term() | nil
%%====================================================================
sl_decode_term(nil, _) -> nil;
sl_decode_term(X, integer) -> list_to_integer(binary_to_list(X));
sl_decode_term(X, float) -> list_to_float(binary_to_list(X));
sl_decode_term(X, _) -> X.
