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
            What = element_at_position(FileSyntaxTree, Line, Column),
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

element_at_position(FileSyntaxTree, Line, Column) ->
    Fun = fun
        ({call, {L, StartColumn}, {atom, {L, StartColumn}, Function}, Args}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {local_call, Function, length(Args)};
                true -> undefined
            end;
        ({call, {_, _}, {remote, {_, _}, {atom, {L, StartColumn}, Module}, {atom, {_, _}, Function}}, Args}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Module)) + 1 + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {remote_call, Module, Function, length(Args)};
                true -> undefined
            end;
        (_SyntaxTree) ->
            undefined
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).

find_element({local_call, Function, Arity}, FileSyntaxTree, File) ->
    case find_function(FileSyntaxTree, Function, Arity) of
        {Line, Column} -> {File, Line, Column};
        _ -> undefined
    end;
find_element({remote_call, Module, Function, Arity}, _FileSyntaxTree, File) ->
    OtherFile = filename:dirname(File) ++ "/" ++ atom_to_list(Module) ++ ".erl",
    OtherFileSyntaxTree = file_syntax_tree(OtherFile), % TODO search in project not in the file directory
    case find_function(OtherFileSyntaxTree, Function, Arity) of
        {Line, Column} -> {OtherFile, Line, Column};
        _ -> undefined
    end.

find_function(FileSyntaxTree, Function, Arity) ->
    Fun = fun
        ({function, Position, FoundFunction, FoundArity, _Clauses}) when FoundFunction =:= Function andalso FoundArity =:= Arity ->
            Position;
        (_SyntaxTree) ->
            undefined
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).
