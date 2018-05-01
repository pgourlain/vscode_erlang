-module(lsp_navigation).

-export([goto_definition/3]).

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
            Module = list_to_atom(filename:basename(File)),
            What = element_at_position(Module, FileSyntaxTree, Line, Column),
            case find_element(What, FileSyntaxTree, File) of
                {FoundFile, FoundLine, FoundColumn} ->
                    #{result => <<"ok">>, uri => list_to_binary("file://" ++ FoundFile), line => FoundLine, character => FoundColumn};
                _ ->
                    #{result => <<"ko">>}
            end;
        _ ->
            #{result => <<"ko">>}
    end.

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

find_element({function_use, Module, Function, Arity}, CurrentFileSyntaxTree, CurrentFile) ->
    CurrentModule = list_to_atom(filename:basename(CurrentFile)),
    {File, SyntaxTree} = case Module of
        CurrentModule ->
            {CurrentFile, CurrentFileSyntaxTree};
        _ ->
            OtherFile = filename:dirname(CurrentFile) ++ "/" ++ atom_to_list(Module) ++ ".erl",
            {OtherFile, file_syntax_tree(OtherFile)} % TODO search in project not in the file directory
    end,
    case find_function(SyntaxTree, Function, Arity) of
        {Line, Column} -> {File, Line, Column};
        _ -> undefined
    end;
find_element({variable, Variable, Line, Column}, CurrentFileSyntaxTree, CurrentFile) ->
    AllClauses = lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun (SyntaxTree, SingleAcc) ->
            case SyntaxTree of
                {clause, {_, _}, _, _, _} ->
                    [SyntaxTree | SingleAcc];
                _ ->
                    SingleAcc
            end
        end, Acc, TopLevelSyntaxTree)
    end, [], CurrentFileSyntaxTree),
    ClausesWithVariable = lists:keysort(2, lists:filter(fun (ClauseSyntaxTree) ->
        lists:member({Line, Column}, find_variable_in_tree(Variable, ClauseSyntaxTree))
    end, AllClauses)),
    case ClausesWithVariable of
        [_H | _T] ->
            Occurrences = find_variable_in_tree(Variable, lists:last(ClausesWithVariable)),
            case Occurrences of
                [{L, C} | _Tail] ->
                    {CurrentFile, L, C};
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

find_variable_in_tree(Variable, Tree) ->
    lists:sort(find_variable_in_tree(Variable, Tree, [])).

find_variable_in_tree(Variable, {var, {Line, Column}, Variable}, Acc) ->
    [{Line, Column} | Acc];
find_variable_in_tree(Variable, T, Acc) when is_tuple(T) ->
    find_variable_in_tree(Variable, tuple_to_list(T), Acc);
find_variable_in_tree(Variable, [H | T], Acc) ->
     find_variable_in_tree(Variable, T, find_variable_in_tree(Variable, H, Acc));
find_variable_in_tree(_Variable, _, Acc) ->
    Acc.

find_function(FileSyntaxTree, Function, Arity) ->
    Fun = fun
        ({function, Position, FoundFunction, FoundArity, _Clauses}) when FoundFunction =:= Function andalso FoundArity =:= Arity ->
            Position;
        (_SyntaxTree) ->
            undefined
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).
