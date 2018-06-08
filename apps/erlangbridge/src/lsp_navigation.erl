-module(lsp_navigation).

-export([goto_definition/3, hover_info/3, function_description/2, function_description/3, references_info/3, codelens_info/1,
    find_function_with_line/2, get_function_arities/2, find_record/2]).

goto_definition(File, Line, Column) ->
    FileSyntaxTree = lsp_syntax:file_syntax_tree(File),
    Module = list_to_atom(filename:rootname(filename:basename(File))),
    What = element_at_position(Module, FileSyntaxTree, Line, Column),
    case find_element(What, FileSyntaxTree, File) of
        {FoundFile, FoundLine, FoundColumn} ->
            #{
                uri => lsp_utils:file_uri_to_vscode_uri(list_to_binary("file://" ++ FoundFile)),
                range => lsp_utils:client_range(FoundLine, FoundColumn, FoundColumn)
            };
        _ ->
            throw(<<"Definition not found">>)
    end.

hover_info(File, Line, Column) ->
    Module = list_to_atom(filename:rootname(filename:basename(File))),
    What = element_at_position(Module, lsp_syntax:file_syntax_tree(File), Line, Column),
    case What of
        {function_use, FunctionModule, Function, Arity} ->
            #{contents => function_description(FunctionModule, Function, Arity)};
        _ ->
            #{contents => <<>>}
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
    SyntaxTreeFile = lsp_syntax:module_syntax_tree(Module),                    
    case SyntaxTreeFile of
        {SyntaxTree, File} ->
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
        _ ->
            Help = gen_lsp_help_server:get_help(Module, Function),
            case Help of
                undefined -> <<>>;
                _ -> Help
            end
    end.

references_info(File, Line, Column) ->
    MapResult = fold_in_file_syntax_tree(
        lsp_syntax:file_syntax_tree(File), 
        #{location => {Line, Column}, references => []}, 
        fun references_analyze/2),
    References = maps:get(references, MapResult),
    case maps:get(ref, MapResult, undefined) of
        undefined -> #{result => <<"ko">>};
        RefKey -> 
            Result = lists:map(fun ({_, {L,C}}) -> 
                #{uri => list_to_binary("file://" ++ File), line => L, character => C}
            end, lists:filter(fun ({K,_L}) -> K =:= RefKey end, References)),
            #{result => <<"ok">>, references => Result}
    end.

references_analyze(SyntaxTree, Map) ->
    {Line, Column} = maps:get(location, Map),
    case SyntaxTree of
    {function, {FLine, FColumn}, FuncName, Arity, _} when FLine =:= Line andalso FColumn =< Column -> 
        FunKey = lists:flatten(io_lib:format("~p/~p", [FuncName,Arity])),
        EndColumn = FColumn + length(atom_to_list(FuncName)),
        NewMap = if 
            Column =< EndColumn ->
                maps:put(ref, FunKey, Map);
            true -> Map
        end,
        NewMap;
    {call, CLocation, {atom,_,FName},Args} -> 
        FunKey1 = lists:flatten(io_lib:format("~p/~p", [FName,length(Args)])),
        References = maps:get(references, Map) ++ [{FunKey1, CLocation}],
        maps:put(references, References, Map);
    _ -> Map
    end.

codelens_info(File) ->
    %filter only defined functions
    MapResult = maps:filter(fun (_K,V) -> maps:is_key(func_name, V) end,
        fold_in_file_syntax_tree(lsp_syntax:file_syntax_tree(File), #{}, fun codelens_analyze/2)),
    lists:map(fun (V) ->
        {Line, Column} = maps:get(location, V),
        Count = maps:get(count, V),
        FName = list_to_binary(atom_to_list(maps:get(func_name, V))),
        #{
            line => Line,
            character => Column,
            data => #{
                count => Count,
                func_name => FName,
                exported => maps:get(exported, V, false)
            }
        }
    end, maps:values(MapResult)).

codelens_analyze(SyntaxTree, Map) ->
    case SyntaxTree of
    {function, Location, FuncName, Arity, _} -> 
        FunKey = lists:flatten(io_lib:format("~p/~p", [FuncName,Arity])),
        codelens_add_or_update_ref(Map, FunKey, FuncName, Location);
    {call,_LocationCall, {atom,_,FName},Args} -> 
        FunKey1 = lists:flatten(io_lib:format("~p/~p", [FName,length(Args)])),
        codelens_add_or_update_refcount(Map, FunKey1, 1);
    {attribute,_,export,Exports} ->
        %export sample : [{start,0},{stop,0}]
        lists:foldl(fun ({FuncName, Arity}, AccMap) ->
            FunKey = lists:flatten(io_lib:format("~p/~p", [FuncName,Arity])),
            codelens_add_or_update_exported(AccMap, FunKey, FuncName, true)
        end, Map, Exports);
    _ -> Map
    end.

codelens_add_or_update_exported(Map, Key, FuncName, Exported) ->
    case maps:get(Key, Map, undefined) of
    undefined -> maps:put(Key, #{exported => Exported, count=> 0, func_name => FuncName}, Map);
    FMap -> 
        NewFMap = maps:put(exported, Exported, FMap), 
        NewFMap1 = maps:put(func_name, FuncName, NewFMap), 
        maps:put(Key, NewFMap1, Map)
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
    "**" ++ atom_to_list(Function) ++ "**(" ++ join_strings(lists:map(fun erl_prettypr:format/1, Args), ", ") ++ ")".

join_strings([], _) ->
    [];
join_strings([String], _) ->
    String;
join_strings([String|Rest], Joiner) ->
    String ++ Joiner ++ join_strings(Rest, Joiner).

fold_in_file_syntax_tree(FileSyntaxTree, StartAcc, Fun) ->
    lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(Fun, Acc, TopLevelSyntaxTree)
        end, StartAcc, FileSyntaxTree).

find_in_file_syntax_tree(FileSyntaxTree, Fun) ->
    fold_in_file_syntax_tree(FileSyntaxTree, {undefined, undefined}, 
        fun (SyntaxTree, {SingleAcc, CurrentFile}) ->
            UpdatedFile = case SyntaxTree of
                {attribute, _, file, {FileName, _}} -> FileName;
                _ -> CurrentFile
            end,
            case SingleAcc of
                undefined ->
                    {Fun(SyntaxTree, UpdatedFile), UpdatedFile};
                _ ->
                    {SingleAcc, CurrentFile}
            end
        end).

element_at_position(CurrentModule, FileSyntaxTree, Line, Column) ->
    Fun = fun
        ({call, {L, StartColumn}, {atom, {L, StartColumn}, Function}, Args}, _) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, CurrentModule, Function, length(Args)};
                true -> undefined
            end;
        ({call, {_, _}, {remote, {_, _}, {atom, {_, MStartColumn}, Module}, {atom, {L, StartColumn}, Function}}, Args}, _) when L =:= Line andalso MStartColumn =< Column ->
            MEndColumn = MStartColumn + length(atom_to_list(Module)), 
            EndColumn = StartColumn + length(atom_to_list(Module)) + 1 + length(atom_to_list(Function)),
            if 
                Column =< MEndColumn -> {module_use, Module};
                true -> if 
                            Column =< EndColumn -> {function_use, Module, Function, length(Args)};
                            true -> undefined
                        end
            end;
        ({'fun',{L, StartColumn}, {function, Function, Arity}}, _) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + 4 + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, CurrentModule, Function, Arity};
                true -> undefined
            end;
        ({'fun', {_, _}, {function, {atom, {_, _}, Module}, {atom, {L, StartColumn}, Function}, {integer, {_, _}, Arity}}}, _) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                Column =< EndColumn -> {function_use, Module, Function, Arity};
                true -> undefined
            end;
        ({var, {L, StartColumn}, Variable}, _) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Variable)),
            if
                Column =< EndColumn -> {variable, Variable, L, StartColumn};
                true -> undefined
            end;
        ({record, {L, StartColumn}, Record, Fields}, _) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Record)) + 1,
            if
                Column =< EndColumn -> {record, Record};
                true -> find_record_field_use(Record, Fields, Column)
            end;
        ({record, {L, StartColumn}, _, Record, Fields}, _) when L =:= Line andalso StartColumn =< Column ->
            EndColumn = StartColumn + length(atom_to_list(Record)) + 1,
            if
                Column =< EndColumn -> {record, Record};
                true -> find_record_field_use(Record, Fields, Column)
            end;
        ({record_field, {L, RecordSttartColumn}, _, Record, {atom, {L, FieldStartColumn}, Field}}, _) when L =:= Line andalso RecordSttartColumn =< Column ->
            FieldEndColumn = FieldStartColumn + length(atom_to_list(Field)),
            if
                Column < FieldStartColumn -> {record, Record};
                Column < FieldEndColumn -> {field, Record, Field};
                true -> undefined
            end;
        (_SyntaxTree, _File) ->
            undefined
    end,
    {What, _File} = find_in_file_syntax_tree(FileSyntaxTree, Fun),
    What.

find_record_field_use(_Record, [], _Column) ->
    undefined;
find_record_field_use(Record, [{record_field, _, {atom, {_, StartColumn}, Field}, _} | Tail], Column) ->
    EndColumn = StartColumn + length(atom_to_list(Field)),
    if
        StartColumn =< Column andalso Column =< EndColumn -> {field, Record, Field};
        true -> find_record_field_use(Record, Tail, Column)
    end.

find_element({module_use, Module}, _CurrentFileSyntaxTree, _CurrentFile) ->
    {SyntaxTree, File} = lsp_syntax:module_syntax_tree(Module),
    case find_module(SyntaxTree, Module) of
        {attribute, {Line, Column}, _} -> {File, Line, Column};
        _ -> undefined
    end;
find_element({function_use, Module, Function, Arity}, _CurrentFileSyntaxTree, _CurrentFile) ->
    {SyntaxTree, File} = lsp_syntax:module_syntax_tree(Module),
    case find_function(SyntaxTree, Function, Arity) of
        {function, {Line, Column}, _Function, _Arity, _Clauses} -> {File, Line, Column};
        _ -> undefined
    end;
find_element({record, Record}, CurrentFileSyntaxTree, _CurrentFile) ->
    case find_record(CurrentFileSyntaxTree, Record) of
        {{attribute, {Line, Column}, record, {Record, _}}, File} -> {File, Line, Column};
        undefined -> undefined
    end;
find_element({field, Record, Field}, CurrentFileSyntaxTree, _CurrentFile) ->
    case find_record(CurrentFileSyntaxTree, Record) of
        {{attribute, _, record, {Record, Fields}}, File} -> find_record_field(Field, Fields, File);
        undefined -> undefined
    end;
find_element({variable, Variable, Line, Column}, CurrentFileSyntaxTree, CurrentFile) ->
    FunctionWithVariable = find_function_with_line(CurrentFileSyntaxTree, Line),
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
    end;
find_element(_, _CurrentFileSyntaxTree, _CurrentFile) ->
    undefined.

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

get_function_arities(Module, Function) ->
    case lsp_syntax:module_syntax_tree(Module) of
        {FileSyntaxTree, _File} ->
            lists:sort(fold_in_file_syntax_tree(FileSyntaxTree, [], fun
                ({function, _Position, FoundFunction, Arity, _Clauses}, Acc) when FoundFunction =:= Function ->
                    [Arity | Acc];
                (_, Acc) ->
                    Acc
            end));
        _ ->
            []
    end.

find_function(FileSyntaxTree, Function, Arity) ->
    Fun = fun (SyntaxTree, _File) ->
        case SyntaxTree of 
        {function, _Position, FoundFunction, FoundArity, _Clauses} when FoundFunction =:= Function andalso FoundArity =:= Arity ->
            SyntaxTree;
        _ ->
            undefined
        end
    end,
    {SyntaxTree, _File} = find_in_file_syntax_tree(FileSyntaxTree, Fun),
    SyntaxTree.

find_module(FileSyntaxTree, Module) ->
    case lists:keyfind(module, 3, FileSyntaxTree) of
    {attribute, Position, module, Module} -> {attribute, Position, Module};
    _ -> undefined
    end.

find_record(FileSyntaxTree, Record) ->
    Fun = fun (SyntaxTree, _File) ->
        case SyntaxTree of
            {attribute, _, record, {Record, _}} -> SyntaxTree;
            _ -> undefined
        end
    end,
    find_in_file_syntax_tree(FileSyntaxTree, Fun).

find_record_field(_Field, [], _CurrentFile) ->
    undefined;
find_record_field(Field, [{record_field, _, {atom, {Line, Column}, Field}} | _], CurrentFile) ->
    {CurrentFile, Line, Column};
find_record_field(Field, [_ | Tail], CurrentFile) ->
    find_record_field(Field, Tail, CurrentFile).
