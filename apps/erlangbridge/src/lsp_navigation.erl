-module(lsp_navigation).

-export([goto_definition/3, hover_info/3, find_rebar_config/1, references_info/3, codelens_info/1]).

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
                                    DocAsString = edoc:layout(edoc:get_doc(_File, [{hidden, true}, {private, true}]), 
                                        [{layout, hover_doc_layout}, {filter, [{function, {Function, Arity}}]} ]),                            
                                    %error_logger:info_msg("Documentation : ~p~n", [DocAsString]),                                
                                                                        
                                    FunctionHeaders = join_strings(lists:map(fun ({clause, _Location, Args, _Guard, _Body}) ->
                                        function_header(Function, Args)
                                    end, Clauses), "  \n") ++ "  \n" ++ DocAsString,
                                    #{result => <<"ok">>, text => list_to_binary(FunctionHeaders)};
                                _ ->
                                    %check if a BIF
                                    case lists:keyfind(Function,1, erlang:module_info(exports)) of
                                    {Function, _} -> 
                                        #{result => <<"ok">>, moduleName => list_to_binary("erlang"), functionName => list_to_binary(atom_to_list(Function))};
                                    _  -> #{result => <<"ko">>}
                                    end
                            end;                                
                        _ ->
                            #{result => <<"ok">>, moduleName => list_to_binary(atom_to_list(FunctionModule)), functionName => list_to_binary(atom_to_list(Function))}
                    end;
                _ ->
                    #{result => <<"ko">>}
            end
    end.

references_info(File, Line, Column) ->
    try internal_references_info(File, Line, Column) of
        _Any -> _Any
    catch
        _Err:_Reason -> error_logger:info_msg("references_info error ~p:~p", [_Err, _Reason])
    end.

internal_references_info(File, _Line, _Column) ->
    #{result => <<"ok">>, references => [
        #{uri => list_to_binary("file://" ++ File), line => 1, character => 1},
        #{uri => list_to_binary("file://" ++ File), line => 10, character => 1}        
    ]}.

codelens_info(File) ->
    try internal_codelens_info(File) of
        _Any -> _Any
    catch
        _Err:_Reason -> error_logger:info_msg("codelens_info error ~p:~p", [_Err, _Reason])
    end.

internal_codelens_info(File) ->
    case file_syntax_tree(File) of
        undefined -> #{result => <<"ko">>};
        FileSyntaxTree ->
            %filter only defined functions
            MapResult = maps:filter(fun (_K,V) -> maps:is_key(func_name, V) end,
                fold_in_file_syntax_tree(FileSyntaxTree, #{}, fun codelens_analyze/2)),
            Result = lists:map(fun (V) ->
                {Line, Column} = maps:get(location, V),
                Count = maps:get(count, V),
                FName = list_to_binary(atom_to_list(maps:get(func_name, V))),
                #{
                    line => Line, character => Column,                    
                    data => #{
                        count => Count,
                        func_name => FName
                    }
                }
            end, maps:values(MapResult)),
            %io:format("MapResult : ~p~n", [Result]),
            #{result => <<"ok">>, codelens => Result, uri => list_to_binary("file://" ++ File)}
    end.

codelens_analyze(SyntaxTree, Map) ->
    case SyntaxTree of
    {function, Location, FuncName, Arity, _} -> 
        FunKey = lists:flatten(io_lib:format("~p/~p", [FuncName,Arity])),
        codelens_add_or_update_ref(Map, FunKey, FuncName, Location);
    {call,_LocationCall, {atom,_,FName},Args} -> 
        FunKey1 = lists:flatten(io_lib:format("~p/~p", [FName,length(Args)])),
        codelens_add_or_update_refcount(Map, FunKey1, 1);
    _ -> Map
    end.

codelens_add_or_update_ref(Map, Key, FuncName, Location) ->
    case maps:get(Key, Map, undefined) of
    undefined -> maps:put(Key, #{location => Location, count=> 0, func_name => FuncName}, Map);
    FMap -> 
        NewFMap = maps:put(location, Location, FMap), 
        NewFMap1 = maps:put(func_name, FuncName, NewFMap), 
        maps:put(Key, NewFMap1, Map)
    end.
    
codelens_add_or_update_refcount(Map, Key, Count) ->
    case maps:get(Key, Map, undefined) of
    undefined -> maps:put(Key, #{count=> Count}, Map);
    FMap ->
        NewCount = maps:get(count, FMap) + Count,
        NewFMap = maps:put(count, NewCount, FMap), 
        maps:put(Key, NewFMap, Map)
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

fold_in_file_syntax_tree(FileSyntaxTree, StartAcc, Fun) ->
    lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(Fun, Acc, TopLevelSyntaxTree)
        end, StartAcc, FileSyntaxTree).

find_in_file_syntax_tree(FileSyntaxTree, Fun) ->
    fold_in_file_syntax_tree(FileSyntaxTree, undefined, 
        fun (SyntaxTree, SingleAcc) ->
            case SingleAcc of
                undefined ->
                    Fun(SyntaxTree);
                _ ->
                    SingleAcc
            end
        end).

element_at_position(CurrentModule, FileSyntaxTree, Line, Column) ->
    Fun = fun
        ({call, {L, StartColumn}, {atom, {L, StartColumn}, Function}, Args}) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, CurrentModule, Function, length(Args)};
                true -> undefined
            end;
        ({call, {_, _}, {remote, {_, _}, {atom, {_, MStartColumn}, Module}, {atom, {L, StartColumn}, Function}}, Args}) when L =:= Line andalso MStartColumn =< Column ->
            MEndColumn = MStartColumn + length(atom_to_list(Module)), 
            EndColumn = StartColumn + length(atom_to_list(Module)) + 1 + length(atom_to_list(Function)),
            if 
                Column =< MEndColumn -> {module_use, Module};
                true -> if 
                            Column =< EndColumn -> {function_use, Module, Function, length(Args)};
                            true -> undefined
                        end
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
            ModuleFile = find_module_file(Module, CurrentFile),
            case ModuleFile of
                undefined ->
                    undefined;
                _ ->
                    {file_syntax_tree(ModuleFile), ModuleFile}
            end
    end.

find_module_file(Module, CurrentFile) ->
    CurrentDir = filename:dirname(CurrentFile),
    RebarConfig = find_rebar_config(CurrentDir),
    RootDir = case RebarConfig of
        undefined -> CurrentDir;
        _ -> filename:dirname(RebarConfig)
    end,
    Found = filelib:fold_files(RootDir, atom_to_list(Module) ++ ".erl", true, fun (Found, Acc) ->
        [Found | Acc]
    end, []),
    case Found of
        [] ->
            undefined;
        [OneFile] ->
            OneFile;
        [AFile|_] ->
            BuildElements = filename:split(RootDir) ++ ["_build"],
            NoBuildFiles = lists:filter(fun (Filename) ->
                not lists:prefix(BuildElements, filename:split(Filename))
            end, Found),
            case NoBuildFiles of
                [ANoBuioldFile|_] ->
                    ANoBuioldFile;
                _ ->
                    AFile
            end
    end.

find_rebar_config(CurrentDir) ->
    RebarConfig = filename:join(CurrentDir, "rebar.config"),
    case filelib:is_file(RebarConfig) of
        true ->
            RebarConfig;
        _ ->
            Elements = filename:split(CurrentDir),
            case Elements of
                [_] ->
                    undefined;
                _ ->
                    find_rebar_config(filename:join(lists:droplast(Elements)))
            end
    end.

find_element({module_use, Module}, CurrentFileSyntaxTree, CurrentFile) ->
    {SyntaxTree, File} = get_module_syntax_tree(Module, CurrentFileSyntaxTree, CurrentFile),
    case find_module(SyntaxTree, Module) of
        {attribute, {Line, Column}, _} -> {File, Line, Column};
        _ -> undefined
    end;
    
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
        case SyntaxTree of 
        {function, _Position, FoundFunction, FoundArity, _Clauses} when FoundFunction =:= Function andalso FoundArity =:= Arity ->
            SyntaxTree;
        _ ->
            undefined
        end
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).

find_module(FileSyntaxTree, Module) ->
    case lists:keyfind(module, 3, FileSyntaxTree) of
    {attribute, Position, module, Module} -> {attribute, Position, Module};
    _ -> undefined
    end.
