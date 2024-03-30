-module(lsp_navigation).

-export([definition/3, hover_info/3, function_description/2, function_description/3, references/3]).
-export([codelens_info/1, symbol_info/1, record_fields/2, find_function_with_line/2, fold_references/4]).
-export([inlayhints_info/3, full_inlayhints_info/2, functions/2]).
-export([inlinevalues_info/2]).

-define(LOG(S),
	begin
        gen_lsp_server:lsp_log("~p", [S])
	end).
-define(LOG(Fmt, Args),
	begin
        gen_lsp_server:lsp_log(Fmt, Args)
	end).

-type lsp_location() :: {File::file:filename(), LineNo::pos_integer(),
                         FirstColumn::pos_integer(), LastColumn::pos_integer()}.

-spec definition(File::file:filename(),
                 Line::pos_integer(),
                 Column::pos_integer()) -> [lsp_location()].
definition(File, Line, Column) ->
    find_definition(File, find_at(File, Line, Column)).

hover_info(File, Line, Column) ->
    case find_at(File, Line, Column) of
        {{reference, {function, Module, Function, Arity}}, _Details} ->
            function_description(Module, Function, Arity);
        _ ->
            undefined
    end.

references(File, Line, Column) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    case find_at(File, Line, Column) of
        {{_, {function, Module, Function, Arity}}, _Details} ->
            Local = case FileModule =:= Module of
                true -> local_function_references(File, Function, Arity);
                false -> []
            end,
            Global = [{F, L, C, E} || [F, L, C, E] <- gen_lsp_doc_server:get_references({function, Module, Function, Arity})],
            Global ++ Local;
        {{variable, {Variable, VariableLine, VariableColumn}}, _Details} ->
            Length = length(atom_to_list(Variable)),
            [{File, L, C, C + Length} || {L, C} <- variable_references(File, Variable, VariableLine, VariableColumn)];
        _ ->
            []
    end.

codelens_info(File) ->
    {Functions, Exports} = fold_in_syntax_tree(fun
        ({function, {L, Start}, Function, Arity, _}, CurrentFile, {FunAcc, ExportAcc}) when CurrentFile =:= File ->
            {[{Function, Arity, L, Start} | FunAcc], ExportAcc};
        ({attribute,_, export, Exports}, _CurrentFile, {FunAcc, ExportAcc}) ->
            UpdatedExportAcc = lists:foldl(fun ({Function, Arity}, Acc) ->
                Acc#{{Function, Arity} => true}
            end, ExportAcc, Exports),
            {FunAcc, UpdatedExportAcc};
        (_SyntaxTree, _CurrentFile, Acc) ->
            Acc
    end, {[], #{}}, File, gen_lsp_doc_server:get_syntax_tree(File)),
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    lists:map(fun ({Function, Arity, L, Start}) ->
        RefCount = length(local_function_references(File, Function, Arity)) +
            length(gen_lsp_doc_server:get_references({function, FileModule, Function, Arity})),
        {Function, RefCount, maps:is_key({Function, Arity}, Exports), L, Start}
    end, Functions).

%
% return { Position, Label} => {{0,0}, "sample"}
%
inlayhints_info(File, {LS, _CS}, {LE,_CE}) ->
    %get inlayhints from store
    Res = gen_lsp_doc_server:get_inlayhints(File),
    %?LOG("inlayhint_info expected range:(~p,~p),(~p,~p)", [LS,CS, LE,CE]),
    % filter base on line number
    FilteredRes = lists:filter(fun ({{L,_},_,_}) -> L >= LS andalso L =< LE end ,Res),
    %?LOG("inlayhint_info result:~p, filtered:~p", [length(Res), length(FilteredRes)]),
    FilteredRes.

full_inlayhints_info(File, SyntaxTree) ->
    % return empty if not enabled -> optimize background parsing
    case gen_lsp_config_server:inlayHintsEnabled() of
    false -> [];
    _ ->
        #{defs := Defs, calls := Calls} = fold_in_syntax_tree(fun lsp_inlayhints:inlayhint_analyze/3,
                    #{defs => [], calls => []},
                    File, SyntaxTree),
        %?LOG("full_inlayhints_info result:~p, ~p", [Defs, Calls]),
        NewInlays = [X || X <- lsp_inlayhints:generate_inlayhints(Calls, Defs)],
        NewInlays
    end.

%
% return { Kind, Position, Label} => {{0,0}, "sample"}
%
inlinevalues_info(File, {LS, _CS}) ->

    Funs = fold_in_syntax_tree(
        fun
            ({function, {L, _}, _FName, _Arity, _}=Tree, _CurrentFile, Acc) 
              when L =< LS  
            -> [Tree | Acc];
            (_, _, Acc) -> Acc
        end, 
        [],
        File,
        gen_lsp_doc_server:get_syntax_tree(File)
    ),
    case Funs of
        [] -> 
            %?LOG("no function found at stoppedLocation", []),
            [];
        [E|_] ->
            Res = find_variables_and_expression(E),
            %remove duplicates
            Res1 = lists:usort(Res),
            %?LOG("inlinevalues_info:~p", [Res1]),
            Res1
    end.

%%inspired from erl_syntax_lib:variables
find_variables_and_expression(Tree) ->
    find_variables_and_expression(Tree, []).

find_variables_and_expression(T, Acc) ->
    case erl_syntax:type(T) of
        variable ->
            %filter based on line number
            case T of
                {_, {_, _}, _}  ->
                    [T | Acc];
                _ ->
                    Acc
            end;
        macro ->
            %% macro names are ignored, even if represented by variables
            case erl_syntax:macro_arguments(T) of
                none -> Acc;
                As -> variables_2(As, Acc)
            end;
        application ->
            % {call ,...} -> look for expressions
            case erl_syntax:application_arguments(T) of
                [] -> Acc;
                As -> expressions_1(As, Acc)
            end;
        _ ->
            case erl_syntax:subtrees(T) of
                [] ->
                    Acc;
                Gs ->
                    variables_1(Gs, Acc)
            end
    end.

variables_1([L | Ls], S) ->
    variables_1(Ls, variables_2(L, S));
variables_1([], S) ->
    S.

variables_2([T | Ts], S) ->
    variables_2(Ts, find_variables_and_expression(T, S));
variables_2([], S) ->
    S.

expressions_1([T | Ts], S) ->
    expressions_1(Ts, expressions_2(T, S));
expressions_1([], S) ->
    S.

expressions_2({op, Position, _, _, _} = T, S) ->
    [{expression, Position, erl_prettypr:format(T)} | S];
expressions_2({op, Position, _, _, _, _} = T, S) ->
    [{expression, Position, erl_prettypr:format(T)} | S];
expressions_2(_, S) ->
    S.


symbol_info(File) ->
    lists:reverse(fold_in_syntax_tree(fun
        (_SyntaxTree, CurrentFile, Acc) when CurrentFile =/= File ->
            Acc;
        ({function, {L, _}, Function, Arity, _}, _CurrentFile, Acc) ->
            FullName = iolist_to_binary(io_lib:format("~p/~p", [Function, Arity])),
            [{FullName, 12, L} | Acc];
        ({attribute, {L, _}, record, {Record, _}}, _CurrentFile, Acc) ->
            [{Record, 23, L} | Acc];
        (_SyntaxTree, _CurrentFile, Acc) ->
            Acc
    end, [], File, gen_lsp_doc_server:get_syntax_tree(File))).

%return list of function for specified file
functions(File, FunName) ->
    fold_in_syntax_tree(fun
            ({function, {_, _}, FName, _Arity, _}=F, _CurrentFile, Acc) when FName =:= FunName ->
                [F | Acc];
            (_SyntaxTree, _CurrentFile, Acc) ->
                Acc
        end,
        [], File, gen_lsp_doc_server:get_syntax_tree(File)).

record_fields(File, Record) ->
    RecordFields = find_in_syntax_tree(fun
        ({attribute, _, record, {FoundRecord, Fields}}, _CurrentFile) when FoundRecord =:= Record -> 
            Fields;
        (_SyntaxTree, _CurrentFile) ->
            undefined
    end, File),
    case RecordFields of
        undefined ->
            [];
        _ ->
            lists:map(fun
                FieldGetter({record_field, _, {atom, _, Field}}) -> Field;
                FieldGetter({record_field, _, {atom, _, Field}, _}) -> Field;
                FieldGetter({typed_record_field, RecordField, _Type}) -> FieldGetter(RecordField)
            end, RecordFields)
    end.

find_at(File, Line, Column) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    LineContents = file_line(File, Line),
    case find_with_re(File, LineContents, Column) of
        undefined ->
            find_in_syntax_tree(fun
                (_SyntaxTree, CurrentFile) when CurrentFile =/= File ->
                    undefined;
                ({call, {L, Start}, {atom, {L, Start}, Function}, Args}, _CurrentFile) when L == Line, Column >= Start ->
                    case column_in_atom(Function, Start, Column) of
                        true -> {{reference, {function, FileModule, Function, length(Args)}}, [{args, Args}]};
                        false -> undefined
                    end;
                ({attribute, {_L, _StartColumn}, export, _Exports}, _CurrentFile) ->
                    find_exported_function(FileModule, LineContents, Column);
                ({call, {_, _}, {remote, {_, _}, {atom, {_, ModuleStart}, Module}, {atom, {L, Start}, Function} = Prefix}, Args}, _CurrentFile) ->
                    case L == Line andalso column_in_atom(Module, ModuleStart, Column) of
                        true ->
                            {{reference, {module, Module}}, []};
                        false ->
                            case L == Line andalso column_in_atom(Function, Start, Column) of
                                true -> {{reference, {function, Module, Function, length(Args)}}, [{args, Args}]};
                                false -> find_in_args(Module, Function, Prefix, Args, Line, Column)
                            end
                    end;
                ({'fun',{L, Start}, {function, Function, Arity}}, _CurrentFile) when L == Line, Start =< Column ->
                    case column_in_atom(Function, Start, Column) of
                        true -> {{reference, {function, FileModule, Function, Arity}}, []};
                        false -> undefined
                    end;
                ({'fun', {_, _}, {function, {atom, {_, _}, Module}, {atom, {L, Start}, Function}, {integer, {_, _}, Arity}}}, _CurrentFile)
                        when L == Line, Start =< Column ->
                    case column_in_atom(Function, Start, Column) of
                        true -> {{reference, {function, Module, Function, Arity}}, []};
                        false -> undefined
                    end;
                ({var, {L, Start}, Variable}, _CurrentFile) when L == Line, Start =< Column ->
                    case column_in_atom(Variable, Start, Column) of
                        true -> {{variable, {Variable, L, Start}}, []};
                        false -> undefined
                    end;
                ({record, {L, Start}, Record, Fields}, _CurrentFile) when L == Line, Start =< Column ->
                    case column_in_atom(Record, Start, Column) of
                        true -> {{reference, {record, Record}}, []};
                        false -> find_record_field(Record, Fields, Column, Line)
                    end;
                ({record, _, Record, Fields}, _CurrentFile) ->
                    find_record_field(Record, Fields, Column, Line);
                ({record, {L, Start}, _, Record, Fields}, _CurrentFile) when L == Line, Start =< Column ->
                    case column_in_atom(Record, Start, Column) of
                        true -> {{reference, {record, Record}}, []};
                        false -> find_record_field(Record, Fields, Column, Line)
                    end;
                ({record, _, _, Record, Fields}, _CurrentFile) ->
                    find_record_field(Record, Fields, Column, Line);
                ({record_field, {L, RecordStart}, _, Record, {atom, {L, FieldStart}, _}}, _CurrentFile) when L == Line, RecordStart =< Column, Column < FieldStart-1 ->
                    %% E.g. Foo#fo|o.bar ('|': cursor location)
                    {{reference, {record, Record}}, []};
                ({record_field, _, _, Record, {atom, {L, Start}, Field}}, _CurrentFile) when L == Line, Start =< Column ->
                    case column_in_atom(Field, Start, Column) of
                        true -> {{reference, {field, Record, Field}}, []};
                        false -> undefined
                    end;
                ({record_index, {L, RecordStart}, Record, {atom, {L, Start}, Field}}, _CurrentFile) when L == Line, RecordStart =< Column ->
                    End = Start + length(atom_to_list(Field)),
                    if
                        Column < Start -> {{reference, {record, Record}}, []};
                        Column =< End -> {{reference, {field, Record, Field}}, []};
                        true -> undefined
                    end;
                ({function, _LC, Function, Arity, Clauses}, _CurrentFile) ->
                    lists:foldl(fun
                        ({clause, {L, Start}, _, _, _}, undefined) when L =:= Line ->
                            case column_in_atom(Function, Start, Column) of
                                true -> {{definition, {function, FileModule, Function, Arity}}, []};
                                false -> undefined
                            end;
                        (_Clause, Acc) ->
                            Acc
                    end, undefined, Clauses);
                ({tuple, _LC, [{atom, {ModL, ModC}, ModAtom}, {atom, {FuncL, FuncC}, FuncAtom} | _]}, _CurrentFile) when ModL == Line; FuncL == Line ->
                    module_function_atoms({ModL, ModC}, ModAtom, {FuncL, FuncC}, FuncAtom, Line, Column);
                (_SyntaxTree, _CurrentFile) ->
                    undefined
            end, File);
        FoundWithRE ->
            {FoundWithRE, []}
    end.

fold_in_syntax_tree(_Fun, StartAcc, _File, undefined) ->
    StartAcc;
fold_in_syntax_tree(Fun, StartAcc, File, SyntaxTree) ->
    {Result, _, _} = lists:foldl(fun (TopLevelElementSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun
                        ({attribute, _, file, {NewFile, _}} = Tree, {ValueAcc, _CurrentFile, undefined}) ->
                            {Fun(Tree, File, ValueAcc), File, NewFile};
                        ({attribute, _, file, {NewFile, _}} = Tree, {ValueAcc, _CurrentFile, FirstFile}) when NewFile =:= FirstFile ->
                            {Fun(Tree, File, ValueAcc), File, FirstFile};
                        ({attribute, _, file, {NewFile, _}} = Tree, {ValueAcc, _CurrentFile, FirstFile}) ->
                            {Fun(Tree, NewFile, ValueAcc), NewFile, FirstFile};
                        (Tree, {ValueAcc, CurrentFile, FirstFile}) ->
                            {Fun(Tree, CurrentFile, ValueAcc), CurrentFile, FirstFile}
        end, Acc, TopLevelElementSyntaxTree)
    end, {StartAcc, File, undefined}, SyntaxTree),
    Result.

find_in_syntax_tree(Fun, File) ->
    fold_in_syntax_tree(fun
        (SyntaxTree, CurrentFile, undefined) -> Fun(SyntaxTree, CurrentFile);
        (_SyntaxTree, _CurrentFile, Value) -> Value
    end, undefined, File, gen_lsp_doc_server:get_syntax_tree(File)).

find_exported_function(_CurrentModule, undefined, _Column) ->
    undefined;
find_exported_function(CurrentModule, LineContents, Column) ->
    case re:run(LineContents, <<"([a-z][A-Z_0-9a-z@]*)/([0-9]+)">>, [global]) of
        {match, Matches} ->
            case lists:filter(fun ([{Pos, Len}, _, _]) -> Column > Pos andalso Column =< Pos + Len end, Matches) of
                [[_, {NamePos, NameLen}, {ArityPos, ArityLen}]] ->
                    Function = lsp_utils:bin_to_atom(binary:part(LineContents, NamePos, NameLen), latin1),
                    Arity = binary_to_integer(binary:part(LineContents, ArityPos, ArityLen)),
                    {{reference, {function, CurrentModule, Function, Arity}}, []};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

find_in_args(Module, Function, Prefix, Args, Line, Column) ->
    GenFun = fun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg) ->
        case Line == GenMsgLine andalso GenMsgColumn =< Column andalso column_in_atom(AtomMsg, GenMsgColumn, Column) of
            true when Module =:= gen_server ->
                {{gen_msg, [GenMsg]}, []};
            true when Module =:= gen_statem ->
                PreMsg = case Function of
                    call -> {tuple, {0, 0}, [Prefix, {var, {0, 0}, 'From'}]};
                    _ -> Prefix
                end,
                {{gen_msg, [PreMsg, GenMsg]}, []};
            _ ->
                undefined
        end
    end,
    Result = case Args of
        [_, {atom, {GenMsgLine, GenMsgColumn}, AtomMsg} = GenMsg | _] ->
            GenFun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg);
        [_, {tuple, _, [{atom, {GenMsgLine, GenMsgColumn}, AtomMsg} | _]} = GenMsg | _] ->
            GenFun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg);
        _ ->
            undefined
    end,
    case Result of
        undefined ->
            case Args of
                [{atom, ModAtomLC, ModAtom}, {atom, FuncAtomLC, FuncAtom} | _] ->
                    module_function_atoms(ModAtomLC, ModAtom, FuncAtomLC, FuncAtom, Line, Column);
                _ ->
                    undefined
            end;
        _ ->
            Result
    end.

module_function_atoms({Line, ModAtomC}, _ModAtom, {Line, _FuncAtomC}, _FuncAtom, Line, Column) when Column < ModAtomC ->
    undefined;
module_function_atoms({Line, ModAtomC}, ModAtom, {Line, FuncAtomC}, FuncAtom, Line, Column) ->
    case column_in_atom(ModAtom, ModAtomC, Column) of
        true ->
            {{reference, {module, ModAtom}}, []};
        false ->
            case column_in_atom(FuncAtom, FuncAtomC, Column) of
                true -> {{reference, {function, ModAtom, FuncAtom, any}}, []};
                false -> undefined
            end
    end;
module_function_atoms({Line, ModAtomC}, ModAtom, {_FuncAtomL, _FuncAtomC}, _FuncAtom, Line, Column) ->
    case column_in_atom(ModAtom, ModAtomC, Column) of
        true -> {{reference, {module, ModAtom}}, []};
        false -> undefined
    end;
module_function_atoms({_ModAtomL, _ModAtomC}, ModAtom, {Line, FuncAtomC}, FuncAtom, Line, Column) ->
    case column_in_atom(FuncAtom, FuncAtomC, Column) of
        true -> {{reference, {function, ModAtom, FuncAtom, any}}, []};
        false -> undefined
    end;
module_function_atoms({_ModAtomL, _ModAtomC}, _ModAtom, {_FuncAtomL, _FuncAtomC}, _FuncAtom, _Line, _Column) ->
    undefined.

find_record_field(_Record, [], _Column, _Line) ->
    undefined;
find_record_field(Record, [{record_field, _, {atom, {L, Start}, Field}, _} | Tail], Column, Line) when L == Line, Start =< Column ->
    case column_in_atom(Field, Start, Column) of
        true -> {{reference, {field, Record, Field}}, []};
        false -> find_record_field(Record, Tail, Column, Line)
    end;
find_record_field(Record, [_ | Tail], Column, Line) ->
    find_record_field(Record, Tail, Column, Line).

find_with_re(_File, undefined, _Column) ->
    undefined;
find_with_re(File, LineContents, Column) ->
    case find_macro_reference(LineContents, Column) of
        undefined -> find_include(File, LineContents, Column);
        MacroReference -> MacroReference
    end.

find_macro_reference(LineContents, Column) ->
    case re:run(LineContents, <<"\\?[A-Z_0-9a-z]+">>, [global]) of
        {match, Matches} ->
            case lists:filter(fun ([{Pos, Len}]) -> Column > Pos andalso Column =< Pos + Len end, Matches) of
                [[{FoundPos, FoundLen}]] ->
                    Macro = lsp_utils:bin_to_atom(binary:part(LineContents, FoundPos + 1, FoundLen - 1), latin1),
                    {reference, {macro, Macro}};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

find_include(File, LineContents, Column) ->
    case re:run(LineContents, <<"-(include|include_lib)\\(\"([^\"]+)\"\\)\\.">>, [global]) of
        {match, Matches} ->
            IncludeFiles =
                lists:foldl(
                    fun([{Start, Len}, AttributePL, FilenamePL], []) when Column - 1 >= Start, Column < Start + Len ->
                        case binary:part(LineContents, AttributePL) of
                            <<"include">> -> resolve_include_file_paths(File, binary:part(LineContents, FilenamePL));
                            <<"include_lib">> -> [find_libdir(binary:part(LineContents, FilenamePL))]
                        end;
                    (_, Acc) ->
                        Acc
                    end,
                    [],
                    Matches),
            case [IncFile || IncFile<-IncludeFiles, IncFile /= undefined] of
                [] -> undefined;
                IncFiles -> {include, IncFiles}
            end;
        _ ->
            undefined
    end.

column_in_atom(_Atom, Start, Column) when Column < Start->
    false;
column_in_atom(Atom, Start, Column) ->
    Column =< Start + length(atom_to_list(Atom)).

resolve_include_file_paths(File, IncludeFileName) ->
    IncludePaths = lsp_parse:get_include_path(File),
    Candidates = [filename:join(Path, IncludeFileName) || Path <- IncludePaths],
    lists:filter(fun filelib:is_file/1, Candidates).

find_libdir(IncludeFileName) ->
    case filename:split(IncludeFileName) of
        [LibName | Remain] ->
            case code:lib_dir(lsp_utils:bin_to_atom(LibName)) of
                {error, bad_name} ->
                    gen_lsp_doc_server:find_source_file(IncludeFileName);
                AbsLib ->
                    filename:nativename(filename:join([AbsLib | Remain]))
            end;
        _ ->
            undefined
    end.

local_function_references(File, Function, Arity) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    Definition = find_definition(File, {{reference, {function, FileModule, Function, Arity}}, []}),
    case Definition of
        [] ->
            []; % Probably BIF
        _ ->
            FunctionLength = length(atom_to_list(Function)),
            fold_in_syntax_tree(fun
                ({call, {L, C}, {atom, _, FunctionName}, Args}, CurrentFile, Acc)
                        when CurrentFile =:= File, Function =:= FunctionName, length(Args) == Arity ->
                    [{File, L, C, C + FunctionLength} | Acc];
                (_SyntaxTree, _CurrentFile, Acc) ->
                    Acc
            end, [], File, gen_lsp_doc_server:get_syntax_tree(File))
    end.

find_definition(File, What) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    find_definition(File, FileModule, What).

-spec find_definition(File, FileModule, {ItemToFind, Details}) -> Result
      when File :: file:filename(),
           FileModule :: module(),
           ItemToFind :: term(),
           Details :: term(),
           Result :: [lsp_location()].
find_definition(File, Module, {{_, {module, Module}}, _Details}) ->
    [{File, 1, 1, 1}];
find_definition(_File, _FileModule, {{_, {module, Module}}, _Details}) ->
    [{ModuleFile, 1, 1, 1}
     || ModuleFile<-gen_lsp_doc_server:get_module_files(Module)];
find_definition(_File, _FileModule, {{include, IncludedFiles = [_|_]}, _Details}) ->
    [{IncludedFile, 1, 1, 1} || IncludedFile<-IncludedFiles];
find_definition(File, FileModule, {{_, {function, Module, Function, Arity}}, Details}) ->
    FindFun =
        fun({function, {L, Start}, FoundFunction, FoundArity, Clauses}, CurrentFile)
                when Function =:= FoundFunction andalso (Arity =:= any orelse Arity == FoundArity) ->
            case proplists:get_value(args, Details) of
                undefined ->
                    {CurrentFile, L, Start, Start};
                Args ->
                    case find_clause_location(Clauses, Args) of
                        {CL, CC} -> {CurrentFile, CL, CC, CC};
                        undefined -> {CurrentFile, L, Start, Start}
                    end
            end;
        (_SyntaxTree, _CurrentFile) ->
            undefined
        end,
    ModFiles =
        if  FileModule == Module -> [File];
            true                 -> gen_lsp_doc_server:get_module_files(Module)
        end,
    Locations = [find_in_syntax_tree(FindFun, ModFile) || ModFile<-ModFiles],
    [Location || Location<-Locations, Location /= undefined];
find_definition(File, _FileModule, {{gen_msg, GenMsg}, _Details}) ->
    case match_gen_msg(File, GenMsg) of
        undefined -> [];
        {ClauseLine, ClauseColumn} -> [{File, ClauseLine, ClauseColumn, ClauseColumn}]
    end;
find_definition(File, _FileModule, {{_, {record, Record}}, _Details}) ->
    find_definitions(record, Record, File);
find_definition(File, _FileModule, {{_, {field, Record, Field}}, _Details}) ->
    find_definitions(field, {Record, Field}, File);
find_definition(File, _FileModule, {{variable, {Variable, Line, Column}}, _Details}) ->
    case variable_references(File, Variable, Line, Column) of
        [] -> [];
        [{L, C} | _] -> [{File, L, C, C}]
    end;
find_definition(File, _FileModule, {{_, {macro, 'MODULE'}}, _Details}) ->
    [{File, 1, 1, 1}];
find_definition(File, _FileModule, {{_, {macro, Macro}}, _Details}) ->
    find_definitions(macro, Macro, File);
find_definition(_File, _FileModule, _What) ->
    [].

variable_references(File, Variable, Line, Column) ->
    FunctionWithVariable = find_function_with_line(gen_lsp_doc_server:get_syntax_tree(File), Line),
    DoClause = fun ({clause, ClauseLC, _, _, _} = Clause, ClauseAcc) ->
        VarLCs = erl_syntax_lib:fold(fun
            ({var, LC, V}, Acc) when V =:= Variable -> [LC | Acc];
            (_, Acc) -> Acc
        end, [], Clause),
        lists:foldl(fun (VarLC, Acc) ->
            case Acc of
                #{VarLC := _} -> Acc;
                _ -> Acc#{VarLC => ClauseLC}
            end
        end, ClauseAcc, VarLCs)
    end,
    VarLC2ClauseLC = erl_syntax_lib:fold(fun
        ({function, _FunctionLC, _Function, _Arity, Clauses}, Acc) ->
            lists:foldl(DoClause, Acc, Clauses);
        ({'fun', _FunLC, {clauses, Clauses}}, Acc) ->
            lists:foldl(DoClause, Acc, Clauses);
        (_S, ClauseAcc) ->
            ClauseAcc
    end, #{}, FunctionWithVariable),
    variablelc_to_clauselc(VarLC2ClauseLC, Line, Column).


-if(?OTP_RELEASE >= 23).
variablelc_to_clauselc(VarLC2ClauseLC, Line, Column) ->
    case VarLC2ClauseLC of
        #{{Line, Column} := ClauseLC} ->
            lists:usort(lists:filtermap(fun
                ({VarLC, ThisClauseLC}) when ThisClauseLC =:= ClauseLC -> {true, VarLC};
                (_) -> false
            end, maps:to_list(VarLC2ClauseLC)));
        _ ->
            []
    end.
-else.
% support for Erlang 22
variablelc_to_clauselc(VarLC2ClauseLC, Line, Column) ->
    case maps:get({Line, Column}, VarLC2ClauseLC, none) of
	    none -> [];
	    ClauseLC ->
            lists:usort(lists:filtermap(fun
                ({VarLC, ThisClauseLC}) when ThisClauseLC =:= ClauseLC -> {true, VarLC};
                (_) -> false
            end, maps:to_list(VarLC2ClauseLC)))
    end.
-endif.

%% @doc Find all definition of a macro, record or record field.
-spec find_definitions(Type, Name::atom(), File::file:filename())
            -> [lsp_location()]
      when Type :: macro | record | field.
find_definitions(Type, Name, File) ->
    find_definitions_in_files(Type, Name, [File], #{}).

-spec find_definitions_in_files(Type, Name, FilesToVisit, VisitedFiles)
            -> [lsp_location()]
      when Type :: macro | record | field,
           Name :: atom(),
           FilesToVisit :: [file:filename()],
           VisitedFiles :: #{file:filename() => 1}.
find_definitions_in_files(_Type, _Name, [], _VisitedFiles) ->
    [];
find_definitions_in_files(Type, Name, [File | Files], VisitedFiles) ->
    Forms = gen_lsp_doc_server:get_dodged_syntax_tree(File),
    case find_definition_in_file(Type, Name, File, Forms) of
        undefined ->
            %% Go deeper (check included files on the next level)
            IncludedRelFiles = lists:reverse(find_included_files(Forms, [])),
            IncludeDirs = lsp_parse:get_include_path(File),
            PossibleIncludeFiles = [filename:join(Dir, IncludedRelFile)
                                    || IncludedRelFile <- IncludedRelFiles,
                                       Dir <- IncludeDirs] ++ Files,
            IncludeFilesToVisit = [IncFile
                                   || IncFile <- PossibleIncludeFiles,
                                      not maps:is_key(IncFile, VisitedFiles)],
            find_definitions_in_files(Type, Name, IncludeFilesToVisit,
                                      VisitedFiles#{File => 1});
        Result ->
            %% Don't go deeper (ignore included files on the next level) but
            %% go sideway (check e.g. build target dependent alternative files)
            [Result | find_definitions_in_files(Type, Name, Files,
                                                VisitedFiles#{File => 1})]
    end.

%% @doc Find macro, record or record field definition in a dodged AST
-spec find_definition_in_file(Type, Name, File, AST) -> Result
      when Type :: macro | record | field,
           Name :: atom(),
           File :: file:filename(),
           AST :: undefined | [term()],
           Result :: undefined | lsp_location().
find_definition_in_file(_Type, _Name, _File, undefined) ->
    undefined;
find_definition_in_file(_Type, _Name, _File, []) ->
    undefined;
find_definition_in_file(macro, MacroName, File,
                        [{tree, attribute, _,
                          {attribute, {atom, _, define},
                           [{_, Line, MacroName}, _]}}
                         | _Forms]) ->
    {File, Line, 1, 1};
find_definition_in_file(macro, MacroName, File,
                        [{tree, attribute, _,
                          {attribute, {atom, _, define},
                           [{_, _, _, {_, {_, Line, MacroName}, _}}, _]}}
                         | _Forms]) ->
    {File, Line, 1, 1};
find_definition_in_file(record, RecordName, File,
                        [{tree, attribute, _,
                          {attribute, {tree, atom, _, record},
                           [{tree, atom, {attr, Line, _, _}, RecordName}, _]}}
                         | _Forms]) ->
    {File, Line, 1, 1};
find_definition_in_file(field, {RecordName, FieldName}, File,
                        [{tree, attribute, _,
                          {attribute, {tree, atom, _, record},
                           [{tree, atom, _, RecordName},
                            {tree, tuple, _, FieldTrees}]}}
                         | _Forms]) ->
    find_field_definition_in_file(FieldName, File, FieldTrees);
find_definition_in_file(Type, Name, File, [_ | Forms]) ->
    find_definition_in_file(Type, Name, File, Forms).

%% @doc Find record field definition in a sub-list of dodged AST
-spec find_field_definition_in_file(FieldName::atom(),
                                    File::file:filename(),
                                    AST::[term()])
                            -> undefined | lsp_location().
find_field_definition_in_file(_Name, _File, []) ->
    undefined;
find_field_definition_in_file(Name, File,
                              [{tree, record_field, _,
                                {record_field, {atom, Line, Name}, _}}
                               | _Forms]) ->
    {File, Line, 1, 1};
find_field_definition_in_file(Name, File,
                              [{tree,typed_record_field, _,
                                {typed_record_field,
                                 {tree, record_field, _,
                                  {record_field, {atom, Line, Name}, _}}, _}}
                               | _Forms]) ->
    {File, Line, 1, 1};
find_field_definition_in_file(Name, File, [_ | Forms]) ->
    find_field_definition_in_file(Name, File, Forms).

find_included_files(undefined, Acc) -> Acc;
find_included_files([], Acc) -> Acc;
find_included_files([{tree, attribute, _, {attribute, {atom, _, include_lib}, [{string, _ , Name}]}} | Tail], Acc) ->
    find_included_files(Tail, [Name | Acc]);
find_included_files([{tree, attribute, _, {attribute, {atom, _, include}, [{string, _ , Name}]}} | Tail], Acc) ->
    find_included_files(Tail, [Name | Acc]);
find_included_files([_ | Tail], Acc) -> find_included_files(Tail, Acc).

file_line(File, Line) ->
    Contents = case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, FileContents} = file:read_file(File),
            FileContents;
        StoredContents ->
            StoredContents
    end,
    Lines = binary:split(Contents, <<"\n">>, [global]),
    try
        lists:nth(Line, Lines)
    catch _:_ ->
        undefined
    end.

function_description(Module, Function) ->
    Help = gen_lsp_help_server:get_help(Module, Function),
    case Help of
        undefined ->
            function_description(Module, Function, any);
        _ ->
            Help
    end.

function_description(Module, Function, Arity) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            get_generic_help(Module, Function);
        File ->
            case lsp_utils:is_erlang_lib_file(File) of
                false ->
                    case function_clauses(File, Function, Arity) of
                        [] ->
                            case lists:keyfind(Function, 1, erlang:module_info(exports)) of
                                {Function, _} -> gen_lsp_help_server:get_help(erlang, Function);
                                _  -> <<>>
                            end;
                        ClausesList ->
                            iolist_to_binary(lists:map(fun (Clauses) ->
                                DocAsString = try edoc:layout(edoc:get_doc(File, [{hidden, true}, {private, true}]), 
                                    [{layout, hover_doc_layout}, {filter, [{function, {Function, Arity}}]} ]) of
                                        _Any -> _Any
                                    catch
                                        _Err:_Reason -> ""
                                    end,                                                                        
                                FunctionHeaders = join_strings(lists:map(fun ({clause, _Location, Args, _Guard, _Body}) ->
                                    function_header(Function, Args)
                                end, Clauses), "  \n") ++ "  \n" ++ DocAsString,
                                list_to_binary(FunctionHeaders)
                            end, ClausesList))
                    end;
                _ ->
                    get_generic_help(Module, Function)
            end
    end.

get_generic_help(Module, Function) ->
    Help = gen_lsp_help_server:get_help(Module, Function),
    case Help of
        undefined -> <<>>;
        _ -> Help
    end.

function_header(Function, Args) ->
    "**" ++ atom_to_list(Function) ++ "**(" ++ join_strings(lists:map(fun erl_prettypr:format/1, Args), ", ") ++ ")".

join_strings([], _) ->
    [];
join_strings([String], _) ->
    String;
join_strings([String|Rest], Joiner) ->
    String ++ Joiner ++ join_strings(Rest, Joiner).

find_clause_location([{clause, Location, ClauseArgs, _, _} | TailClauses], Args) ->
    case comparison_args(Args, ClauseArgs) of
        false -> find_clause_location(TailClauses, Args);
        true -> Location
    end;
find_clause_location([], _Args) -> undefined.

comparison_args([{Type, _, Value1} | T1], [{Type, _, Value2} | T2]) when is_list(Value1), is_list(Value2) ->
    case comparison_args(Value1, Value2) of
        true -> comparison_args(T1, T2);
        false -> false
    end;
comparison_args([{var, _, _} | T1], [_ | T2]) ->
    comparison_args(T1, T2);
comparison_args([{cons, _, _, _} | T1], [_ | T2]) ->
    comparison_args(T1, T2);
comparison_args([{nil, _} | T1], [_ | T2]) ->
    comparison_args(T1, T2);
comparison_args([{record, _, _, _, _} | T1], [_ | T2]) ->
    comparison_args(T1, T2);
comparison_args([{Type, _, Value} | T1], [{Type, _, Value} | T2]) ->
    comparison_args(T1, T2);
comparison_args([], _) -> true;
comparison_args(_, _) -> false.

find_function_with_line(FileSyntaxTree, Line) ->
    AllFunctionsInReverseOrder = lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun (SyntaxTree, SingleAcc) ->
            case SyntaxTree of
                {function, {_, _}, _, _, _} ->
                    [SyntaxTree | SingleAcc];
                _ ->
                    SingleAcc
            end
        end, Acc, TopLevelSyntaxTree)
    end, [], FileSyntaxTree),
    FunctionWithLineList = lists:dropwhile(fun ({function, {FunctionStartLine, _}, _, _, _}) ->
         FunctionStartLine > Line
    end, AllFunctionsInReverseOrder),
    case FunctionWithLineList of
        [FunctionWithLine | _] -> FunctionWithLine;
        _ -> undefined
    end.

match_gen_msg(File, GenMsg) ->
    find_in_syntax_tree(fun
        ({function, _Position, _FoundFunction, _FoundArity, Clauses}, CurrentFile) when CurrentFile =:= File ->
            find_clause_location(Clauses, GenMsg);
        (_SyntaxTree, _CurrentFile) ->
            undefined
    end, File).

function_clauses(File, Function, Arity) ->
    lists:reverse(fold_in_syntax_tree(fun
        ({function, _LC, FoundFunction, FoundArity, Clauses}, _CurrentfFile, Acc)
                when FoundFunction =:= Function andalso (Arity =:= any orelse FoundArity == Arity) ->
            [Clauses | Acc];
        (_SyntaxTree, _CurrentFile, Acc) ->
            Acc
    end, [], File, gen_lsp_doc_server:get_syntax_tree(File))).

fold_references(Fun, Init, File, FileSyntaxTree) ->
    fold_in_syntax_tree(fun
        (_Syntax, CurrentFile, Acc) when CurrentFile =/= File ->
            Acc;
        ({call, {_, _}, {remote, {_, _}, {atom, {ModuleLine, ModuleColumn}, Module}, {atom, {_L, FunctionColumn}, Function}}, Args}, _CurrentFile, Acc) ->
            End = FunctionColumn + length(atom_to_list(Function)),
            Fun({function, Module, Function, length(Args)}, ModuleLine, ModuleColumn, End, Acc);
        ({'fun', {_, _}, {function, {atom, {ModuleLine, ModuleColumn}, Module}, {atom, {_, _}, Function}, {integer, {_, Start}, Arity}}}, _CurrentFile, Acc) ->
            End = Start + length(integer_to_list(Arity)),
            Fun({function, Module, Function, Arity}, ModuleLine, ModuleColumn, End, Acc);
        (_Syntax, _CurrentFIle, Acc) ->
            Acc
    end, Init, File, FileSyntaxTree).
