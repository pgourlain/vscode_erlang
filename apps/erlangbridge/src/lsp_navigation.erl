-module(lsp_navigation).

-export([goto_definition/3, hover_info/3]).

goto_definition(File, Line, Column) ->
    try internal_goto_definition(File, Line, Column) of
        _Any -> _Any
    catch
        _Err:_Reason -> error_logger:info_msg("goto_definition error ~p:~p", [_Err, _Reason])
    end.

internal_goto_definition(File, Line, Column) ->
    FileSyntaxTree = file_syntax_tree(File),
    case FileSyntaxTree of
        undefined ->
            #{result => <<"ko">>};
        _ ->
            Module = list_to_atom(filename:rootname(filename:basename(File))),
            What = element_at_position(Module, FileSyntaxTree, Line, Column),
            case find_element(What, FileSyntaxTree, File) of
                {FoundFile, FoundLine, FoundColumn} ->
                    #{result => <<"ok">>, uri => list_to_binary("file://" ++ FoundFile), line => FoundLine, character => FoundColumn};
                _ ->
                    #{result => <<"ko">>}
            end
    end.

hover_info(File, Line, Column) ->
    try internal_hover_info(File, Line, Column) of
        _Any -> _Any
    catch
        _Err:_Reason -> error_logger:info_msg("hover_info error ~p:~p", [_Err, _Reason])
    end.

internal_hover_info(File, Line, Column) ->
    FileSyntaxTree = file_syntax_tree(File),
    case FileSyntaxTree of
        undefined ->
            #{result => <<"ko">>};
        _ ->
            Module = list_to_atom(filename:rootname(filename:basename(File))),
            What = element_at_position(Module, FileSyntaxTree, Line, Column),
            case What of
                {function_use, FunctionModule, Function, Arity} ->
                    SyntaxTreeFile = get_module_syntax_tree(FunctionModule, FileSyntaxTree, File),
                    case SyntaxTreeFile of
                        {SyntaxTree, _File} ->
                            case find_function(SyntaxTree, Function, Arity) of
                                {function, _, _, _, Clauses} ->
                                    FunctionHeaders = join_strings(lists:map(fun ({clause, _Location, Args, _Guard, _Body}) ->
                                        function_header(Function, Args)
                                    end, Clauses), "  \n"),
                                    #{result => <<"ok">>, text => list_to_binary(FunctionHeaders)};
                                _ ->
                                    #{result => <<"ko">>}
                            end;                                
                        _ ->
                            #{result => <<"ok">>, moduleName => list_to_binary(atom_to_list(FunctionModule)), functionName => list_to_binary(atom_to_list(Function))}
                    end;
                _ ->
                    #{result => <<"ko">>}
            end
    end.

function_header(Function, Args) ->
    atom_to_list(Function) ++ "(" ++ join_strings(lists:map(fun erl_prettypr:format/1, Args), ", ") ++ ")".

join_strings([], _) ->
    [];
join_strings([String], _) ->
    String;
join_strings([String|Rest], Joiner) ->
    String ++ Joiner ++ join_strings(Rest, Joiner).

file_syntax_tree(File) ->
    case gen_lsp_doc_server:get_document(File) of
        {ok, FileSyntaxTree} ->
            FileSyntaxTree;
        not_found -> 
            case lsp_syntax:parse(File) of
                {ok, FileSyntaxTree} ->
                    FileSyntaxTree;
                _ ->
                    undefined
            end        
    end.

find_in_file_syntax_tree(FileSyntaxTree, Fun) ->
    lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun (SyntaxTree, SingleAcc) ->
            case SingleAcc of
                undefined ->
                    Fun(SyntaxTree);
                _ ->
                    SingleAcc
            end
        end, Acc, TopLevelSyntaxTree)
    end, undefined, FileSyntaxTree).

element_at_position(CurrentModule, FileSyntaxTree, Line, Column) ->
    Fun = fun
        ({call, {L, StartColumn}, {atom, {L, StartColumn}, Function}, Args}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, CurrentModule, Function, length(Args)};
                true -> undefined
            end;
        ({call, {_, _}, {remote, {_, _}, {atom, {_, _}, Module}, {atom, {L, StartColumn}, Function}}, Args}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Module)) + 1 + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, Module, Function, length(Args)};
                true -> undefined
            end;
        ({'fun',{L, StartColumn}, {function, Function, Arity}}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + 4 + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, CurrentModule, Function, Arity};
                true -> undefined
            end;
        ({'fun', {_, _}, {function, {atom, {_, _}, Module}, {atom, {L, StartColumn}, Function}, {integer, {_, _}, Arity}}}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, Module, Function, Arity};
                true -> undefined
            end;
        ({var, {L, StartColumn}, Variable}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Variable)),
            if
                Column =< EndColumn -> {variable, Variable, L, StartColumn};
                true -> undefined
            end;
        (_SyntaxTree) ->
            undefined
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).

get_module_syntax_tree(Module, CurrentFileSyntaxTree, CurrentFile) ->
    CurrentModule = list_to_atom(filename:basename(CurrentFile)),
    case Module of
        CurrentModule ->
            {CurrentFileSyntaxTree, CurrentFile};
        _ ->
            OtherFile = filename:dirname(CurrentFile) ++ "/" ++ atom_to_list(Module) ++ ".erl",
            case filelib:is_regular(OtherFile) of
                true ->
                    {file_syntax_tree(OtherFile), OtherFile}; % TODO search in project not in the file directory
                _ ->
                    undefined
            end
    end.

find_element({function_use, Module, Function, Arity}, CurrentFileSyntaxTree, CurrentFile) ->
    {SyntaxTree, File} = get_module_syntax_tree(Module, CurrentFileSyntaxTree, CurrentFile),
    case find_function(SyntaxTree, Function, Arity) of
        {function, {Line, Column}, _Function, _Arity, _Clauses} -> {File, Line, Column};
        _ -> undefined
    end;
find_element({variable, Variable, Line, Column}, CurrentFileSyntaxTree, CurrentFile) ->
    AllFunctionsInReverseOrder = lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun (SyntaxTree, SingleAcc) ->
            case SyntaxTree of
                {function, {_, _}, _, _, _} ->
                    [SyntaxTree | SingleAcc];
                _ ->
                    SingleAcc
            end
        end, Acc, TopLevelSyntaxTree)
    end, [], CurrentFileSyntaxTree),
    [FunctionWithVariable | _] = lists:dropwhile(fun ({function, {FunctionStartLine, _}, _, _, _}) ->
         FunctionStartLine > Line
    end, AllFunctionsInReverseOrder),
    FunClausesShadowingVariable = lists:sort(erl_syntax_lib:fold(fun (SyntaxTree, SingleAcc) ->
        case SyntaxTree of
            {'fun', {_, _}, {clauses, Clauses}} ->
                lists:foldl(fun (Clause, FunClauseAcc) ->
                    FunHasVariable =
                        variable_location_in_fun_clause(Variable, Line, Column, Clause) andalso
                        variable_in_fun_clause_arguments(Variable, Clause),
                    case FunHasVariable of
                        true -> [Clause | FunClauseAcc];
                        _ -> FunClauseAcc
                    end
                end, SingleAcc, Clauses);
            _ ->
                SingleAcc
        end
    end, [], FunctionWithVariable)),
    case FunClausesShadowingVariable of
        [] ->
            case find_variable_occurrences(Variable, FunctionWithVariable) of
                [{L, C} | _Tail] ->
                    {CurrentFile, L, C};
                _ ->
                    undefined
            end;
        _ ->
            [{L, C} | _] = find_variable_occurrences(Variable, lists:last(FunClausesShadowingVariable)),
            {CurrentFile, L, C}
    end.

find_variable_occurrences(Variable, Tree) ->
    lists:sort(find_variable_occurrences(Variable, Tree, [])).

find_variable_occurrences(Variable, {var, {Line, Column}, Variable}, Acc) ->
    [{Line, Column} | Acc];
find_variable_occurrences(Variable, T, Acc) when is_tuple(T) ->
    find_variable_occurrences(Variable, tuple_to_list(T), Acc);
find_variable_occurrences(Variable, [H | T], Acc) ->
     find_variable_occurrences(Variable, T, find_variable_occurrences(Variable, H, Acc));
find_variable_occurrences(_Variable, _, Acc) ->
    Acc.

variable_location_in_fun_clause(Variable, Line, Column, Clause) ->
    lists:member({Line, Column}, find_variable_occurrences(Variable, Clause)).

variable_in_fun_clause_arguments(Variable, {clause, {_, _}, Arguments, _, _}) ->
    length(find_variable_occurrences(Variable, Arguments)) > 0.

find_function(FileSyntaxTree, Function, Arity) ->
    Fun = fun (SyntaxTree) ->
        case SyntaxTree of {function, Position, FoundFunction, FoundArity, _Clauses} when FoundFunction =:= Function andalso FoundArity =:= Arity ->
            SyntaxTree;
        _ ->
            undefined
        end
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).
