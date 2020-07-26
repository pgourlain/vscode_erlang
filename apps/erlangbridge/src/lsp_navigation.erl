-module(lsp_navigation).

-export([goto_definition/3, hover_info/3, function_description/2, function_description/3, references_info/3, codelens_info/1,
    find_function_with_line/2, get_function_arities/2, find_record/2, symbol_info/2]).

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
            %try to find if user press F12 in export attribute
            What1 = read_and_analyse_from_tokens(File, Module, Line, Column), 
            case find_element(What1, FileSyntaxTree, File) of
                {FoundFile1, FoundLine1, FoundColumn1} ->
                    #{
                        uri => lsp_utils:file_uri_to_vscode_uri(list_to_binary("file://" ++ FoundFile1)),
                        range => lsp_utils:client_range(FoundLine1, FoundColumn1, FoundColumn1)
                    };
                _ ->
                    throw(<<"Definition not found">>)
            end
    end.

read_and_analyse_from_tokens(File, Module, Line, Column) ->
    %% parse file to get all tokens with all informations (Line and Column)
    case file:open(File, [read]) of
    {ok, FIO} ->
        FileSize = filelib:file_size(File),
        Ret = case file:read(FIO, FileSize) of
            {ok, Data} ->
                Scan = do_scan(lsp_utils:to_string(Data), [], Line, Column,{1,1}),
                %gen_lsp_server:lsp_log("scan:~p",[Scan]),
                TargetExport = safe_analyse_export(Scan, Module, Line, Column),
                %gen_lsp_server:lsp_log("read_and_analyse_from_tokens:~p",[TargetExport]),
                TargetExport;
            _ -> { error, ""}
        end,
        file:close(FIO),
        Ret;
    _ ->
        {error, could_not_open_file, File}
    end.

-ifdef(OTP_RELEASE).
safe_analyse_export(Scan, Module, Line, Column) ->
    try case analyse_export(Scan, Module, Line, Column) of
        {notfound} -> {notfound};
        Other -> Other 
    end
    catch
        Err:M:STK -> 
            gen_lsp_server:lsp_log("read_and_analyse_from_tokens exception:~p,~p,~p",[Err,M,STK]),
            {notfound}
    end.
-else.
safe_analyse_export(Scan, Module, Line, Column) ->
    try case analyse_export(Scan, Module, Line, Column) of
        {notfound} -> {notfound};
        Other -> Other 
    end
    catch
        Err:M -> 
            gen_lsp_server:lsp_log("read_and_analyse_from_tokens exception:~p,~p,~p",[Err,M,erlang:get_stacktrace()]),
            {notfound}
    end.
-endif.


analyse_export(Tokens, Module, Line, Column) ->
    %first find export attribute
    {_,_,ExportTokens} = lists:foldl(fun(Token, ExportAcc) -> 
            {IsAttrStart, ExportFound, Ts} = ExportAcc,
            case Token of 
                {'-', _} ->
                    if 
                        IsAttrStart andalso ExportFound -> ExportAcc;
                        true  ->  {true, false,  [Token]}
                    end;
                {atom, _, export} -> case IsAttrStart of
                    true -> {true, true, Ts ++ [Token]};
                        _ -> {false, false, Ts}
                    end;
                _ -> if 
                    IsAttrStart andalso ExportFound -> {true, true, Ts ++ [Token]};
                    true -> ExportAcc
                    end
            end
        end, {false, false, []}, Tokens),
    %{'[',{4,9}},{atom,{4,10},functionA},{'/',{4,19}},{integer,{4,20},0},{',',{4,21}},
    %{atom,{5,5},function_test},{'/',{5,18}},{integer,{5,19},0},{']',{5,20}},
     %%keep only export tokens
    T1 = lists:dropwhile(fun(X) -> 
            case X of 
                {'[', _} -> false;
                _ -> true
            end
         end, ExportTokens),
    T2 = lists:takewhile(fun(X) -> 
            case X of 
                {']', _} -> false;
                _ -> true
            end
         end, T1),
    %%Fns = [{functionA,{4,10},0},{function_test,{5,5},0}]
    {_,_,Fns} = lists:foldl(fun(Token, Acc) -> 
            {F,LC, Arr} = Acc,
            case Token of
                {atom, {L,C}, FnAtomAme} -> {FnAtomAme, {L,C}, Arr};
                {integer, _, Arity} -> {1,1, Arr ++ [{F, LC, Arity}]};
                _ -> Acc
            end
        end, {1,1,[]}, T2),
    %gen_lsp_server:lsp_log("analyse_export fns:~p", [Fns]),
    %% filter only Fns that are in line and column
    TargetFns = lists:filter(fun({Atom, {L,C}, _}) ->
        EndColumn = C + length(atom_to_list(Atom)),
        if 
           Line =:= L andalso C =< Column andalso Column =< EndColumn -> true;
           true -> false
        end
        end, Fns),
    %take the first one
    TargetToken = lists:nth(1, TargetFns),        
    %gen_lsp_server:lsp_log("analyse_export TargetToken:~p", [TargetToken]),
    % %% match tokens sequence for attribute
    % Export = [{'-', _}, {atom, export, _}], 
    %{atom,{4,10},functionA}
    case TargetToken of
        {FnName, _, Arity} -> 
            %{L1, C1} = LC,
            {function_use, Module, FnName, Arity};
            %{ok, L1, C1};
        _ -> {notfound}
    end.


%% scan string into tokens until EndLine, EndColumn is reached
do_scan(Data, ScannedTokens, EndLine, EndColumn, StartLocation) ->
    case erl_scan:tokens([], lsp_utils:to_string(Data), StartLocation, []) of
        {done, Result, LeftOverChars} ->
            case Result of
                {ok, Tokens, {L,C}} -> 
                    if
                        L >= EndLine andalso C >= EndColumn -> ScannedTokens;
                        true -> do_scan(LeftOverChars, ScannedTokens ++ Tokens, EndLine, EndColumn, {L,C})
                    end;                    
                _ -> ScannedTokens
            end;
        _ -> ScannedTokens
    end.

hover_info(File, Line, Column) ->
    Module = list_to_atom(filename:rootname(filename:basename(File))),
    FileSyntax = lsp_syntax:file_syntax_tree(File),
    What = element_at_position(Module, FileSyntax, Line, Column),
    %gen_lsp_server:lsp_log("hover_info What:~p", [What]),
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
            end;                              
        _ -> get_generic_help(Module, Function)
    end.

get_generic_help(Module, Function) ->
    Help = gen_lsp_help_server:get_help(Module, Function),
    case Help of
        undefined -> <<>>;
        _ -> Help
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

symbol_info(Uri, File) ->
    %return all symbols for the specified document 
    SyntaxTree = lsp_syntax:file_syntax_tree(File),
    %gen_lsp_server:lsp_log("syntax for symbolinfo : ~p", [SyntaxTree]),
    fold_in_file_syntax_tree(SyntaxTree, [], fun (S, Acc) -> symbolinfo_analyze(Uri, S, Acc) end).

symbolinfo_analyze(Uri, SyntaxTree, List) ->
    case SyntaxTree of
    {function, Location, FuncName, Arity, _} -> 
        FuncFullName = list_to_binary(lists:flatten(io_lib:format("~p/~p", [FuncName,Arity]))),
        List++ create_symbolinfo(FuncFullName, Uri, function, Location);
    {attribute, Location, record, {Record, _}} ->
        List++ create_symbolinfo(Record, Uri, struct, Location);
    {attribute, Location, type, {Type, _, _}} ->
        List++ create_symbolinfo(Type, Uri, class, Location);
    _ -> List
    end.

symbol_kind(ErlangKind) ->
    %mapping a name to number expected by vscode
    case ErlangKind of 
    function -> 9;
    interface -> 11;
    array -> 18;
    namespace -> 19;
    event -> 24;
    string -> 15;
    struct -> 23;
    number -> 16;
    null -> 21;
    constant -> 14;
    class -> 5;
    boolean -> 17;
    _ -> 1
    end.

symbol_container_from_symbol_kind(ErlangKind) ->
    case ErlangKind of
    function -> <<"functions">>;
    class -> <<"types">>;
    struct -> <<"records">>;
    _ -> <<"root">>
    end.

create_symbolinfo(FuncName, Uri, SymbolKind, {L, C}) ->
    %gen_lsp_server:lsp_log("create symbol info, ~p: ~p", [FuncName, Location]),
    %vscode documentation  : https://code.visualstudio.com/docs/extensionAPI/vscode-api#SymbolInformation
    [#{
        containerName => symbol_container_from_symbol_kind(SymbolKind),
        name => FuncName,
        kind => symbol_kind(SymbolKind), 
        location => #{ 
            uri => Uri, 
            range => #{ 
                start => #{ character => C, line => L-1}, 
                <<"end">> => #{ character => C, line => L-1 } 
            } 
        }
    }].

function_header(Function, Args) ->
    "**" ++ atom_to_list(Function) ++ "**(" ++ join_strings(lists:map(fun erl_prettypr:format/1, Args), ", ") ++ ")".

join_strings([], _) ->
    [];
join_strings([String], _) ->
    String;
join_strings([String|Rest], Joiner) ->
    String ++ Joiner ++ join_strings(Rest, Joiner).

fold_in_file_syntax_tree(FileSyntaxTree, StartAcc, Fun) ->
    % FileStyntaxTree is [] of TopLevelStyntaxTree
    % post-order traversal, then
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
        ({attribute, {L, StartColumn}, export, _Exports}, _) when L =:= Line andalso StartColumn =< Column ->
            %% return function_use on export
            %% missing line and column info on exported atoms
            undefined;
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
    %gen_lsp_server:lsp_log("element_at_position:~p", [FileSyntaxTree]),
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
