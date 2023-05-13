-module(lsp_navigation).

-export([definition/3, hover_info/3, function_description/2, function_description/3, references/3]).
-export([codelens_info/1, symbol_info/1, find_function_with_line/2, get_function_arities/2, find_record/2, fold_references/3]).

-define(LOG(S),
	begin
        gen_lsp_server:lsp_log("~p", [S])
	end).
-define(LOG(Fmt, Args),
	begin
        gen_lsp_server:lsp_log(Fmt, Args)
	end).

definition(File, Line, Column) ->
    find_definition(File, find_at(File, Line, Column)).

hover_info(File, Line, Column) ->
    case find_at(File, Line, Column) of
        {reference, {function, Module, Function, Arity}} ->
            function_description(Module, Function, Arity);
        _ ->
            undefined
    end.

references(File, Line, Column) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    case find_at(File, Line, Column) of
        {_, {function, Module, Function, Arity}} ->
            Local = case FileModule =:= Module of
                true -> local_function_references(File, Function, Arity);
                false -> []
            end,
            Global = [{F, L, C, E} || [F, L, C, E] <- gen_lsp_doc_server:get_references({function, Module, Function, Arity})],
            Global ++ Local;
        {variable, {Variable, VariableLine, VariableColumn}} ->
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

create_symbolinfo(FuncName, Uri, SymbolKind, {L, C}) ->
    #{
        name => FuncName,
        kind => SymbolKind, 
        location => #{ 
            uri => Uri, 
            range => #{ 
                start => #{ character => C, line => L-1}, 
                <<"end">> => #{ character => C, line => L-1 } 
            } 
        }
    }.

find_at(File, Line, Column) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    LineContents = file_line(File, Line),
    case find_with_re(File, LineContents, Column) of
        undefined ->
            find_in_syntax_tree(fun
                (_SyntaxTree, CurrentFile) when CurrentFile =/= File ->
                    undefined;
                ({call, {L, Start}, {atom, {L, Start}, Function}, Args}, _CurrentFile) when L == Line, Column >= Start ->
                    End = Start + length(atom_to_list(Function)),
                    if
                        Column =< End -> {reference, {function, FileModule, Function, length(Args)}};
                        true -> undefined
                    end;
                ({attribute, {_L, _StartColumn}, export, _Exports}, _CurrentFile) ->
                    find_exported_function(FileModule, LineContents, Column);
                ({call, {_, _}, {remote, {_, _}, {atom, {_, ModuleStart}, Module}, {atom, {L, Start}, Function} = Prefix}, Args}, _CurrentFile) ->
                    ModuleEnd = ModuleStart + length(atom_to_list(Module)), 
                    End = Start + length(atom_to_list(Function)),
                    if
                        L == Line, ModuleStart =< Column, Column =< ModuleEnd -> {reference, {module, Module}};
                        L == Line, Start =< Column, Column =< End -> {reference, {function, Module, Function, length(Args)}};
                        true -> find_gen_msg(Module, Function, Prefix, Args, Line, Column)
                    end;
                ({'fun',{L, Start}, {function, Function, Arity}}, _CurrentFile) when L == Line, Start =< Column ->
                    End = Start + 4 + length(atom_to_list(Function)),
                    if
                        Column =< End -> {reference, {function, FileModule, Function, Arity}};
                        true -> undefined
                    end;
                ({'fun', {_, _}, {function, {atom, {_, _}, Module}, {atom, {L, Start}, Function}, {integer, {_, _}, Arity}}}, _CurrentFile) when L == Line, Start =< Column ->
                    End = Start + length(atom_to_list(Function)),
                    if
                        Column =< End -> {reference, {function, Module, Function, Arity}};
                        true -> undefined
                    end;
                ({var, {L, Start}, Variable}, _CurrentFile) when L == Line, Start =< Column ->
                    End = Start + length(atom_to_list(Variable)),
                    if
                        Column =< End -> {variable, {Variable, L, Start}};
                        true -> undefined
                    end;
                ({record, {L, Start}, Record, Fields}, _CurrentFile) when L == Line, Start =< Column ->
                    End = Start + length(atom_to_list(Record)),
                    if
                        Column =< End -> {reference, {record, Record}};
                        true -> find_record_field(Record, Fields, Column, Line)
                    end;
                ({record, _, Record, Fields}, _CurrentFile) ->
                    find_record_field(Record, Fields, Column, Line);
                ({record, {L, Start}, _, Record, Fields}, _CurrentFile) when L == Line, Start =< Column ->
                    End = Start + length(atom_to_list(Record)),
                    if
                        Column =< End -> {reference, {record, Record}};
                        true -> find_record_field(Record, Fields, Column, Line)
                    end;
                ({record, _, _, Record, Fields}, _CurrentFile) ->
                    find_record_field(Record, Fields, Column, Line);
                ({record_field, _, _, Record, {atom, {L, Start}, Field}}, _CurrentFile) when L == Line, Start =< Column ->
                    End = Start + length(atom_to_list(Field)),
                    if
                        Column =< End -> {reference, {field, Record, Field}};
                        true -> undefined
                    end;
                ({record_index, {L, RecordStart}, Record, {atom, {L, Start}, Field}}, _CurrentFile) when L == Line, RecordStart =< Column ->
                    End = Start + length(atom_to_list(Field)),
                    if
                        Column < Start -> {reference, {record, Record}};
                        Column =< End -> {reference, {field, Record, Field}};
                        true -> undefined
                    end;
                ({function, {L, Start}, Function, Arity, _}, _CurrentFile) when L =:= Line andalso Start =< Column ->
                    End = Start + length(atom_to_list(Function)),
                    if
                        Column < End -> {definition, {function, FileModule, Function, Arity}};
                        true -> undefined
                    end;
                (_SyntaxTree, _CurrentFile) ->
                    undefined
            end, File);
        FoundWithRE ->
            FoundWithRE
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

fold_in_syntax_tree(_Fun, StartAcc, undefined) ->
    StartAcc;
fold_in_syntax_tree(Fun, StartAcc, SyntaxTree) ->
    lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(Fun, Acc, TopLevelSyntaxTree)
    end, StartAcc, SyntaxTree).

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
                    Function = binary_to_atom(binary:part(LineContents, NamePos, NameLen), latin1),
                    Arity = binary_to_integer(binary:part(LineContents, ArityPos, ArityLen)),
                    {reference, {function, CurrentModule, Function, Arity}};
                _ -> undefined
            end;
        _ ->
            undefined
    end.

find_gen_msg(Module, Function, Prefix, Args, Line, Column) ->
    GenFun = fun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg) ->
        GEndColumn = GenMsgColumn + length(atom_to_list(AtomMsg)),
        case Line == GenMsgLine andalso GenMsgColumn =< Column andalso Column =< GEndColumn of
            true when Module =:= gen_server ->
                {gen_msg, [GenMsg]};
            true when Module =:= gen_statem ->
                PreMsg = case Function of
                    call -> {tuple, {0, 0}, [Prefix, {var, {0, 0}, 'From'}]};
                    _ -> Prefix
                end,
                {gen_msg, [PreMsg, GenMsg]};
            _ ->
                undefined
        end
    end,
    case Args of
        [_, {atom, {GenMsgLine, GenMsgColumn}, AtomMsg} = GenMsg | _] ->
            GenFun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg);
        [_, {tuple, _, [{atom, {GenMsgLine, GenMsgColumn}, AtomMsg} | _]} = GenMsg | _] ->
            GenFun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg);
        _ ->
            undefined
    end.

find_record_field(_Record, [], _Column, _Line) ->
    undefined;
find_record_field(Record, [{record_field, _, {atom, {L, Start}, Field}, _} | Tail], Column, Line) when L == Line, Start =< Column ->
    End = Start + length(atom_to_list(Field)),
    if
        Column =< End -> {reference, {field, Record, Field}};
        true -> find_record_field(Record, Tail, Column, Line)
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
                    Macro = binary_to_atom(binary:part(LineContents, FoundPos + 1, FoundLen - 1), latin1),
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
            IncludeFile = lists:foldl(fun
                ([{Start, Len}, AttributePL, FilenamePL], undefined) when Column - 1 >= Start, Column < Start + Len ->
                    case binary:part(LineContents, AttributePL) of
                        <<"include">> -> resolve_include_file_path(File, binary:part(LineContents, FilenamePL));
                        <<"include_lib">> -> find_libdir(binary:part(LineContents, FilenamePL))
                    end;
                (_, Acc) ->
                    Acc
            end, undefined, Matches),
            case IncludeFile of
                undefined -> undefined;
                _ -> {include, IncludeFile}
            end;
        _ ->
            undefined
    end.

resolve_include_file_path(File, IncludeFileName) ->
    IncludePaths = lsp_parse:get_include_path(File),
    Candidates = [filename:join(Path, IncludeFileName) || Path <- IncludePaths],
    case lists:filter(fun filelib:is_file/1, Candidates) of
        [First|_] -> First;
        _ -> IncludeFileName
    end.

find_libdir(IncludeFileName) ->
    case filename:split(IncludeFileName) of
        [LibName | Remain] ->
            case code:lib_dir(binary_to_atom(LibName)) of
                {error, bad_name} ->
                    gen_lsp_doc_server:find_source_file(IncludeFileName);
                AbsLib ->
                    filename:nativename(string:join([AbsLib | Remain], "/"))
            end;
        _ ->
            undefined
    end.

local_function_references(File, Function, Arity) ->
    FunctionLength = length(atom_to_list(Function)),
    fold_in_syntax_tree(fun
        ({call, {L, C}, {atom, _, FunctionName}, Args}, Acc) when Function =:= FunctionName, length(Args) == Arity ->
            [{File, L, C, C + FunctionLength} | Acc];
        (_SyntaxTree, Acc) ->
            Acc
    end, [], gen_lsp_doc_server:get_syntax_tree(File)).

find_definition(_File, {_, {module, Module}}) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined -> undefined;
        ModuleFile -> {ModuleFile, 1, 1, 1}
    end;
find_definition(_File, {include, IncludedFile}) ->
    {IncludedFile, 1, 1, 1};
find_definition(_File, {_, {function, Module, Function, Arity}}) ->
    find_in_syntax_tree(fun
        ({function, {L, Start}, FoundFunction, FoundArity, _}, CurrentFile) when Function =:= FoundFunction, Arity =:= FoundArity ->
            {CurrentFile, L, Start, Start};
        (_SyntaxTree, _CurrentFile) ->
            undefined
    end, gen_lsp_doc_server:get_module_file(Module));
find_definition(File, {gen_msg_use, GenMsg}) ->
    case match_gen_msg(gen_lsp_doc_server:get_syntax_tree(File), GenMsg) of
        undefined -> undefined;
        {ClauseLine, ClauseColumn} -> {File, ClauseLine, ClauseColumn, ClauseColumn}
    end;
find_definition(File, {_, {record, Record}}) ->
    find_in_syntax_tree(fun
        ({attribute, {L, Start}, record, {FoundRecord, _}}, CurrentFile) when Record =:= FoundRecord ->
            {CurrentFile, L, Start, Start};
        (_SyntaxTree, _CurrentFile) ->
            undefined
    end, File);
find_definition(File, {_, {field, Record, Field}}) ->
    find_in_syntax_tree(fun
        ({attribute, _, record, {FoundRecord, Fields}}, CurrentFile) when Record =:= FoundRecord ->
            find_field_definition(Field, Fields, CurrentFile);
        (_SyntaxTree, _CurrentFile) ->
            undefined
    end, File);
find_definition(File, {variable, {Variable, Line, Column}}) ->
    case variable_references(File, Variable, Line, Column) of
        [] -> undefined;
        [{L, C} | _] -> {File, L, C, C}
    end;
find_definition(File, {_, {macro, Macro}}) ->
    find_macro_definition(Macro, File);
find_definition(_File, _What) ->
    undefined.

find_field_definition(_Field, [], _CurrentFile) ->
    undefined;
find_field_definition(Field, [{typed_record_field, {record_field, _, {atom, {Line, Column}, Field}}, _} | _], CurrentFile) ->
    {CurrentFile, Line, Column, Column};
find_field_definition(Field, [{typed_record_field, {record_field, _, {atom, {Line, Column}, Field}, _}, _} | _], CurrentFile) ->
    {CurrentFile, Line, Column, Column};
find_field_definition(Field, [{record_field, _, {atom, {Line, Column}, Field}} | _], CurrentFile) ->
    {CurrentFile, Line, Column, Column};
find_field_definition(Field, [{record_field, _, {atom, {Line, Column}, Field}, _} | _], CurrentFile) ->
    {CurrentFile, Line, Column, Column};
find_field_definition(Field, [_ | Tail], CurrentFile) ->
    find_field_definition(Field, Tail, CurrentFile).

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
    case VarLC2ClauseLC of
        #{{Line, Column} := ClauseLC} ->
            lists:usort(lists:filtermap(fun
                ({VarLC, ThisClauseLC}) when ThisClauseLC =:= ClauseLC -> {true, VarLC};
                (_) -> false
            end, maps:to_list(VarLC2ClauseLC)));
        _ ->
            []
    end.

find_macro_definition(Macro, File) -> find_macro_definition_in_files(Macro, [File]).

find_macro_definition_in_files(_Macro, []) -> undefined;
find_macro_definition_in_files(Macro, [File | Tail]) ->
    Forms = gen_lsp_doc_server:get_dodged_syntax_tree(File),
    case find_macro_definition(Macro, File, Forms) of
        undefined ->
            Included = lists:reverse(find_included_files(Forms, [])),
            IncludePath = lsp_parse:get_include_path(File),
            case find_macro_definition_in_files(Macro, [filename:join(Path, IncludedFile) || IncludedFile <- Included, Path <- IncludePath]) of
                undefined -> find_macro_definition_in_files(Macro, Tail);
                Result -> Result
            end;
        Result -> Result
    end.

find_macro_definition(_Macro, _File, undefined) ->
    undefined;
find_macro_definition(_Macro, _File, []) ->
    undefined;
find_macro_definition(Macro, File, [{tree, attribute, _, {attribute, {atom, _, define}, [{_, Line, Macro}, _]}} | _]) ->
    {File, Line, 1, 1};
find_macro_definition(Macro, File, [{tree, attribute, _, {attribute, {atom, _, define}, [{_, _, _, {_, {_, Line, Macro}, _}}, _]}} | _]) ->
    {File, Line, 1, 1};
find_macro_definition(Macro, File, [_ | Tail]) ->
    find_macro_definition(Macro, File, Tail).

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
            lists:foldl(fun (Arity, Acc) ->
                Description = function_description(Module, Function, Arity),
                <<Acc/binary, Description/binary>>
            end, <<>>, get_function_arities(Module, Function));
        _ ->
            Help
    end.

function_description(Module, Function, Arity) ->
    case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            get_generic_help(Module, Function);
        File ->
            case gen_lsp_doc_server:get_syntax_tree(File) of
                undefined ->
                    get_generic_help(Module, Function);
                SyntaxTree ->
                    case lsp_utils:is_erlang_lib_file(File) of
                        false ->
                        case find_function(SyntaxTree, Function, Arity) of
                            {function, _, _, _, Clauses} ->
                                DocAsString = try edoc:layout(edoc:get_doc(File, [{hidden, true}, {private, true}]), 
                                    [{layout, hover_doc_layout}, {filter, [{function, {Function, Arity}}]} ]) of
                                        _Any -> _Any
                                    catch
                                        _Err:_Reason -> ""
                                    end,                                                                        
                                FunctionHeaders = join_strings(lists:map(fun ({clause, _Location, Args, _Guard, _Body}) ->
                                    function_header(Function, Args)
                                end, Clauses), "  \n") ++ "  \n" ++ DocAsString,
                                list_to_binary(FunctionHeaders);
                            _ ->
                                %check if a BIF
                                case lists:keyfind(Function,1, erlang:module_info(exports)) of
                                    {Function, _} -> gen_lsp_help_server:get_help(erlang, Function);
                                    _  -> <<>>
                                end
                        end;
                        _ ->  get_generic_help(Module, Function)
                    end
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

find_in_file_syntax_tree(FileSyntaxTree, Fun1) ->
    DefaultVal = case Fun1 of
        {function, Fun} -> {{undefined, undefined}, undefined};
        Fun -> {undefined, undefined}
    end,
    fold_in_syntax_tree(fun (SyntaxTree, {SingleAcc, CurrentFile}) ->
        UpdatedFile = case SyntaxTree of
            {attribute, _, file, {FileName, _}} -> FileName;
            _ -> CurrentFile
        end,
        case SingleAcc of
            undefined ->
                {Fun(SyntaxTree, UpdatedFile), UpdatedFile};
            {undefined, DefaultTree} ->
                {Fun(SyntaxTree, DefaultTree, UpdatedFile), UpdatedFile};
            _ ->
                {SingleAcc, CurrentFile}
        end
    end, DefaultVal, FileSyntaxTree).

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

get_function_arities(Module, Function) ->
    File = gen_lsp_doc_server:get_module_file(Module),
    case gen_lsp_doc_server:get_syntax_tree(File) of
        undefined ->
            [];
        FileSyntaxTree ->
            lists:sort(fold_in_syntax_tree(fun
                ({function, _Position, FoundFunction, Arity, _Clauses}, Acc) when FoundFunction =:= Function ->
                    [Arity | Acc];
                (_, Acc) ->
                    Acc
            end, [], FileSyntaxTree))
    end.

match_gen_msg(FileSyntaxTree, GenMsg) ->
    Fun = fun(SyntaxTree, _File) ->
        case SyntaxTree of 
            {function, _Position, _FoundFunction, _FoundArity, Clauses} ->
                find_clause_location(Clauses, GenMsg);
            _ -> undefined
        end
    end,
    {Location, _File} = find_in_file_syntax_tree(FileSyntaxTree, Fun),
    Location.

find_function(FileSyntaxTree, Function, Arity) ->
    Fun = fun (SyntaxTree, DefaultTree, _File) ->
        case SyntaxTree of 
        {function, _Position, FoundFunction, FoundArity, _Clauses} when FoundFunction =:= Function ->
            case FoundArity of
                Arity -> {SyntaxTree, undefined};
                _ -> {undefined, SyntaxTree}
            end;
        _ ->
            {undefined, DefaultTree}
        end
    end,
    {SyntaxTree1, _File} = find_in_file_syntax_tree(FileSyntaxTree, {function, Fun}),
    case SyntaxTree1 of
        {undefined, SyntaxTree} -> skip;
        {SyntaxTree, _} -> skip
    end,
    SyntaxTree.

find_record(FileSyntaxTree, Record) ->
    Fun = fun (SyntaxTree, _File) ->
        case SyntaxTree of
            {attribute, _, record, {Record, _}} -> SyntaxTree;
            _ -> undefined
        end
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).

fold_references(Fun, Init, FileSyntaxTree) ->
    fold_in_syntax_tree(fun
        ({call, {_, _}, {remote, {_, _}, {atom, {ModuleLine, ModuleColumn}, Module}, {atom, {_FunctionLine, FunctionColumn}, Function}}, Args}, Acc) ->
            End = FunctionColumn + length(atom_to_list(Function)),
            Fun({function, Module, Function, length(Args)}, ModuleLine, ModuleColumn, End, Acc);
        ({'fun', {_, _}, {function, {atom, {ModuleLine, ModuleColumn}, Module}, {atom, {_, _}, Function}, {integer, {_, Start}, Arity}}}, Acc) ->
            End = Start + length(integer_to_list(Arity)),
            Fun({function, Module, Function, Arity}, ModuleLine, ModuleColumn, End, Acc);
        (_Syntax, Acc) ->
            Acc
    end, Init, FileSyntaxTree).