-module(lsp_navigation).

-export([goto_definition/3, hover_info/3, function_description/2, function_description/3, references_info/3, codelens_info/1,
    find_function_with_line/2, get_function_arities/2, find_record/2, symbol_info/2]).

-define(LOG(S),
	begin
        gen_lsp_server:lsp_log("~p", [S])
	end).
-define(LOG(Fmt, Args),
	begin
        gen_lsp_server:lsp_log(Fmt, Args)
	end).

goto_definition(File, Line, Column) ->
    FileSyntaxTree = gen_lsp_doc_server:get_document_syntax_tree(File),
    Module = list_to_atom(filename:rootname(filename:basename(File))),
    What = element_at_position(Module, FileSyntaxTree, Line, Column, file_line(File, Line)),
    ?LOG("element_at_position : ~p", [What]),
    case find_element(What, FileSyntaxTree, File) of
        {FoundFile, FoundLine, FoundColumn} ->
            #{
                uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(FoundFile)),
                range => lsp_utils:client_range(FoundLine, FoundColumn, FoundColumn)
            };
        _ ->
            throw(<<"Definition not found">>)
    end.

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

hover_info(File, Line, Column) ->
    Module = list_to_atom(filename:rootname(filename:basename(File))),
    FileSyntax = gen_lsp_doc_server:get_document_syntax_tree(File),
    What = element_at_position(Module, FileSyntax, Line, Column, file_line(File, Line)),
    %gen_lsp_server:lsp_log("hover_info What:~p", [What]),
    case What of
        {function_use, FunctionModule, Function, Arity} ->
            #{contents => function_description(FunctionModule, Function, Arity)};
        {function_use, FunctionModule, Function, Arity, _Args} ->
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
    File = gen_lsp_doc_server:get_module_file(Module),
    case gen_lsp_doc_server:get_document_syntax_tree(File) of
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
    end.

get_generic_help(Module, Function) ->
    Help = gen_lsp_help_server:get_help(Module, Function),
    case Help of
        undefined -> <<>>;
        _ -> Help
    end.

references_info(File, Line, Column) ->
    MapResult = fold_in_file_syntax_tree(
        gen_lsp_doc_server:get_document_syntax_tree(File), 
        #{location => {Line, Column}, references => []}, 
        fun references_analyze/2),
    References = maps:get(references, MapResult),
    case maps:get(ref, MapResult, undefined) of
        undefined -> #{result => <<"ko">>};
        RefKey -> 
            Result = lists:map(fun ({_, {L,C}}) -> 
                #{uri => lsp_utils:file_to_file_uri(File), line => L, character => C}
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
        fold_in_file_syntax_tree(gen_lsp_doc_server:get_document_syntax_tree(File), #{}, fun codelens_analyze/2)),
    lists:filtermap(fun (V) ->
        case maps:get(location, V, undefined) of
            {Line, Column} ->
                Count = maps:get(count, V),
                FName = list_to_binary(atom_to_list(maps:get(func_name, V))),
                {true, #{
                    line => Line,
                    character => Column,
                    data => #{
                        count => Count,
                        func_name => FName,
                        exported => maps:get(exported, V, false)
                    }
                }};
            undefined -> false
        end
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
    SyntaxTree = gen_lsp_doc_server:get_document_syntax_tree(File),
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

find_in_file_syntax_tree(FileSyntaxTree, Fun1) ->
    DefaultVal = case Fun1 of
        {function, Fun} -> {{undefined, undefined}, undefined};
        Fun -> {undefined, undefined}
    end,
    fold_in_file_syntax_tree(FileSyntaxTree, DefaultVal, 
        fun (SyntaxTree, {SingleAcc, CurrentFile}) ->
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
        end).

element_at_position(CurrentModule, FileSyntaxTree, Line, Column, LineContents) ->
    Fun = fun
        ({tuple, {L, StartTuple}, Args}, _) when L =:= Line, Column > StartTuple ->
            find_definition_in_args(Args, Column, Line, CurrentModule);
        ({cons, {L, StartCons}, _, _} = Cons, _) when L =:= Line, Column > StartCons ->
            case cons_to_list(Cons, []) of
                false -> undefined;
                Args -> find_definition_in_args(Args, Column, Line, CurrentModule)
            end;
        ({call, {L, StartColumn}, {atom, {L, StartColumn}, Function}, Args}, _) ->
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                L =:= Line, StartColumn =< Column, Column =< EndColumn -> {function_use, CurrentModule, Function, length(Args)};
                true -> find_definition_in_args(Args, Column, Line, CurrentModule)
            end;
        ({attribute, {_L, _StartColumn}, export, _Exports}, _) ->
            find_function_in_export(CurrentModule, LineContents, Column);
        ({attribute, _, file, {HrlFile, _}}, _) ->
            process_hrl_filename(HrlFile, LineContents, Column);
        ({call, {_, _}, {remote, {_, _}, {atom, {_, MStartColumn}, Module}, {atom, {L, StartColumn}, Function} = Prefix}, Args}, _) ->
            MEndColumn = MStartColumn + length(atom_to_list(Module)), 
            EndColumn = StartColumn + length(atom_to_list(Function)),
            if
                L =:= Line, MStartColumn =< Column, Column =< MEndColumn -> {module_use, Module};
                L =:= Line, StartColumn =< Column, Column =< EndColumn -> {function_use, Module, Function, length(Args), Args};
                true ->
                    GenFun = fun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg) ->
                        GEndColumn = GenMsgColumn + length(atom_to_list(AtomMsg)),
                        case Line =:= GenMsgLine andalso GenMsgColumn =< Column andalso Column =< GEndColumn of
                            true when Module == gen_server -> {gen_msg_use, [GenMsg]};
                            true when Module == gen_statem ->
                                PreMsg = case Function of
                                    call ->
                                        {tuple, {0, 0}, [Prefix, {var, {0, 0}, 'From'}]};
                                    _ -> Prefix
                                end,
                                {gen_msg_use, [PreMsg, GenMsg]};
                            _ -> false
                        end
                    end,
                    GenResult = case Args of
                        [_, {atom, {GenMsgLine, GenMsgColumn}, AtomMsg} = GenMsg | _] ->
                            GenFun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg);
                        [_, {tuple, _, [{atom, {GenMsgLine, GenMsgColumn}, AtomMsg} | _]} = GenMsg | _] ->
                            GenFun(GenMsgLine, GenMsgColumn, AtomMsg, GenMsg);
                        _ -> false
                    end,
                    case GenResult of
                        false -> find_definition_in_args(Args, Column, Line, CurrentModule);
                        _ -> GenResult
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
        ({record, {L, StartColumn}, Record, Fields}, _) ->
            EndColumn = StartColumn + length(atom_to_list(Record)),
            if
                L =:= Line, StartColumn =< Column, Column =< EndColumn -> {record, Record};
                true -> find_record_field_use(Record, Fields, Column, Line)
            end;
        ({record, {L, StartColumn}, _, Record, Fields}, _) ->
            EndColumn = StartColumn + length(atom_to_list(Record)),
            if
                L =:= Line, StartColumn =< Column, Column =< EndColumn -> {record, Record};
                true -> find_record_field_use(Record, Fields, Column, Line)
            end;
        ({record_field, {L, RecordSttartColumn}, _, Record, {atom, {L, FieldStartColumn}, Field}}, _) when L =:= Line andalso RecordSttartColumn =< Column ->
            FieldEndColumn = FieldStartColumn + length(atom_to_list(Field)),
            if
                Column < FieldStartColumn -> {record, Record};
                Column < FieldEndColumn -> {field, Record, Field};
                true -> undefined
            end;
        ({record_index, {L, RecordSttartColumn}, Record, {atom, {L, FieldStartColumn}, Field}}, _) when L =:= Line andalso RecordSttartColumn =< Column ->
            FieldEndColumn = FieldStartColumn + length(atom_to_list(Field)),
            if
                Column < FieldStartColumn -> {record, Record};
                Column < FieldEndColumn -> {field, Record, Field};
                true -> undefined
            end;
        (_SyntaxTree, _File) ->
            undefined
    end,
    case find_macro_use(LineContents, Column) of
        undefined ->
            case find_in_file_syntax_tree(FileSyntaxTree, Fun) of
                {{function_use, _, _, _} = Export, _File} -> Export;
                {{function_use, _, _, _, _} = Export, _File} -> Export;
                {What, _File} -> What
            end;
        MacroUse -> MacroUse
            
    end.

cons_to_list({nil,_}, List) -> lists:reverse(List);
cons_to_list({cons, _, Data, Cons}, List) ->
    cons_to_list(Cons, [Data|List]);
cons_to_list(_, _List) -> false.

process_hrl_filename(File, LineContents, Column) ->
    MatchResult = find_include_filename(LineContents, Column),
    case MatchResult of
        {include, IncludeFileName, true} ->
            %gen_lsp_server:lsp_log("attr hrl match include", []),
            {hrl, resolve_include_file_path(File, IncludeFileName)};
        {include_lib, IncludeFileName, true} ->
            %gen_lsp_server:lsp_log("attr hrl match include_lib", []),            
            find_libdir(IncludeFileName);
        _ -> undefined
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
        [LibName|Remain] ->
            case code:lib_dir(LibName) of
                {error,bad_name} -> undefined;
                AbsLib -> {hrl, filename:nativename(string:join([AbsLib|Remain], "/")) }
            end;
        _ -> undefined
    end.

find_include_filename(LineContents, Column) ->
    % regular expression, intead of binaries matching. Because a comment can be exists at the end of include line
    case re:run(LineContents, <<"^-include(?<alib>|_lib)\\(\"(?<grp>.+)\"\\)">>, [global,{capture, all_names}]) of
        {match, Matches} ->
            case lists:nth(1,Matches) of
                [{_,0},{Pos,Len}] -> %include
                    IncludeFile =binary_to_list(binary:part(LineContents, Pos, Len)), 
                    %gen_lsp_server:lsp_log("include found: ~p", [IncludeFile]),
                    {include, IncludeFile, is_between(Column, 1, 10+Pos+Len)};
                [{_,_},{Pos,Len}] -> %include_lib
                    IncludeFile = binary_to_list(binary:part(LineContents, Pos, Len)),
                    %gen_lsp_server:lsp_log("include_lib found: ~p", [IncludeFile]),
                    {include_lib, IncludeFile, is_between(Column, 1, 14+Pos+Len)};                  
                _ -> 
                    undefined
            end;
        _ -> undefined
    end.

is_between(C, Start, End) ->
    case C of
    X when X >= Start andalso X =< End -> true;
    _ -> false
    end.
find_macro_use(undefined, _Column) ->
    undefined;
find_macro_use(LineContents, Column) ->
    case re:run(LineContents, <<"\\?[A-Z_0-9a-z]+">>, [global]) of
        {match, Matches} ->
            case lists:filter(fun ([{Pos, Len}]) -> Column > Pos andalso Column =< Pos + Len end, Matches) of
                [[{FoundPos, FoundLen}]] -> {macro_use, binary_to_atom(binary:part(LineContents, FoundPos + 1, FoundLen - 1), latin1)};
                _ -> undefined
            end;
        _ ->
            undefined
    end.

find_definition_in_args([{atom, {ModLine, StartMod}, Module}, {atom, {ColumnLine, StartColumn}, Function}, ArityTuple | _] = [_ | Tail], Column, Line, CurrentModule) ->
    EndMod = StartMod + length(atom_to_list(Module)),
    EndColumn = StartColumn + length(atom_to_list(Function)),
    case Line of
        ModLine when StartMod =< Column, Column =< EndMod, Module =/= CurrentModule ->
            case parse_arity_tuple(ArityTuple, 0) of
                false -> find_definition_in_args(Tail, Column, Line, CurrentModule);
                _Arity -> {module_use, Module}
            end;
        ColumnLine when StartColumn =< Column, Column =< EndColumn ->
            case parse_arity_tuple(ArityTuple, 0) of
                false -> find_definition_in_args(Tail, Column, Line, CurrentModule);
                Arity -> {function_use, Module, Function, Arity}
            end;
        _ -> find_definition_in_args(Tail, Column, Line, CurrentModule)
    end;
find_definition_in_args([_ | Tail], Column, Line, CurrentModule) when length(Tail) > 2 ->
    find_definition_in_args(Tail, Column, Line, CurrentModule);
find_definition_in_args([{atom, {ModLine, StartMod}, Module}, {atom, {ColumnLine, StartColumn}, Function}], Column, Line, CurrentModule)  ->
    EndMod = StartMod + length(atom_to_list(Module)),
    EndColumn = StartColumn + length(atom_to_list(Function)),
    case Line of
        ModLine when StartMod =< Column, Column =< EndMod, Module =/= CurrentModule ->
            {module_use, Module};
        ColumnLine when StartColumn =< Column, Column =< EndColumn ->
            {function_use, Module, Function, 1};
        _ -> undefined
    end;
find_definition_in_args(_, _Column, _Line, _CurrentModule) -> undefined.

parse_arity_tuple({cons, _, _, ArityTuple}, Num) ->
    parse_arity_tuple(ArityTuple, Num + 1);
parse_arity_tuple({nil, _}, Num) -> Num;
parse_arity_tuple({var, _, _}, _Num) -> 1;
parse_arity_tuple({tuple, _, _}, _Num) -> 1;
parse_arity_tuple(_, _) -> false.

find_function_in_export(_CurrentModule, undefined, _Column) ->
    undefined;
find_function_in_export(CurrentModule, LineContents, Column) ->
    case re:run(LineContents, <<"([a-z][A-Z_0-9a-z@]*)/([0-9]+)">>, [global]) of
        {match, Matches} ->
            case lists:filter(fun ([{Pos, Len}, _, _]) -> Column > Pos andalso Column =< Pos + Len end, Matches) of
                [[_, {NamePos, NameLen}, {ArityPos, ArityLen}]] ->
                    Function = binary_to_atom(binary:part(LineContents, NamePos, NameLen), latin1),
                    Arity = binary_to_integer(binary:part(LineContents, ArityPos, ArityLen)),
                    {function_use, CurrentModule, Function, Arity};
                _ -> undefined
            end;
        _ ->
            undefined
    end.

find_record_field_use(_Record, [], _Column, _Line) ->
    undefined;
find_record_field_use(Record, [{record_field, _, {atom, {L, StartColumn}, Field}, _} | Tail], Column, Line) ->
    EndColumn = StartColumn + length(atom_to_list(Field)),
    if
        L =:= Line, StartColumn =< Column, Column =< EndColumn -> {field, Record, Field};
        true -> find_record_field_use(Record, Tail, Column, Line)
    end;
%% Non-atom record field, e.g. #rec{a = 1, _ = '_'} pattern or match spec
find_record_field_use(Record, [_Head | Tail], Column, Line) ->
    find_record_field_use(Record, Tail, Column, Line).

find_element({module_use, Module}, _CurrentFileSyntaxTree, _CurrentFile) ->
    File = gen_lsp_doc_server:get_module_file(Module),
    case gen_lsp_doc_server:get_document_syntax_tree(File) of
        undefined -> undefined;
        SyntaxTree ->
            case find_module(SyntaxTree, Module) of
                {attribute, {Line, Column}, _} -> {File, Line, Column};
                _ -> undefined
            end
    end;
find_element({hrl, HrlFile}, _CurrentFileSyntaxTree, _CurrentFile) ->
    {HrlFile, 1, 1};
find_element({function_use, Module, Function, Arity}, _CurrentFileSyntaxTree, _CurrentFile) ->
    File = gen_lsp_doc_server:get_module_file(Module),
    case gen_lsp_doc_server:get_document_syntax_tree(File) of
        undefined -> undefined;
        SyntaxTree ->
            case find_function(SyntaxTree, Function, Arity) of
                {function, {Line, Column}, _Function, _Arity, _Clauses} -> {File, Line, Column};
                _ -> undefined
            end
    end;
find_element({function_use, Module, Function, Arity, Args}, _CurrentFileSyntaxTree, _CurrentFile) ->
    File = gen_lsp_doc_server:get_module_file(Module),
    case gen_lsp_doc_server:get_document_syntax_tree(File) of
        undefined -> undefined;
        SyntaxTree ->
            case find_function(SyntaxTree, Function, Arity) of
                {function, {Line, Column}, _Function, _Arity, Clauses} ->
                    case find_clause_location(Clauses, Args) of
                        undefined -> {File, Line, Column};
                        {ClauseLine, ClauseColumn} -> {File, ClauseLine, ClauseColumn}
                    end;
                _ -> undefined
            end
    end;
find_element({gen_msg_use, GenMsg}, SyntaxTree, File) ->
    case match_gen_msg(SyntaxTree, GenMsg) of
        undefined -> undefined;
        {ClauseLine, ClauseColumn} -> {File, ClauseLine, ClauseColumn}
    end;
find_element({record, Record}, CurrentFileSyntaxTree, _CurrentFile) ->
    case find_record(CurrentFileSyntaxTree, Record) of
        {{attribute, {Line, Column}, record, {Record, _}}, File} ->
            {lsp_utils:to_string(File), Line, Column};
        undefined ->
            undefined
    end;
find_element({field, Record, Field}, CurrentFileSyntaxTree, _CurrentFile) ->
    case find_record(CurrentFileSyntaxTree, Record) of
        {{attribute, _, record, {Record, Fields}}, File} ->
            find_record_field(Field, Fields, lsp_utils:to_string(File));
        undefined ->
            undefined
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
            {function, _, _, _, ClausesList} ->
                matched_fun_clause(lists:reverse(ClausesList), Line);
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
find_element({macro_use, Macro}, _CurrentFileSyntaxTree, CurrentFile) ->
    lsp_syntax:find_macro_definition(Macro, CurrentFile);
find_element(_, _CurrentFileSyntaxTree, _CurrentFile) ->
    undefined.

matched_fun_clause([{clause, {ClauseLine, _}, _, _, _} = Clause | _], Line) when ClauseLine =< Line -> [Clause];
matched_fun_clause([_ | Tail], Line) -> matched_fun_clause(Tail, Line);
matched_fun_clause([], _line) -> [].

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
    File = gen_lsp_doc_server:get_module_file(Module),
    case gen_lsp_doc_server:get_document_syntax_tree(File) of
        undefined ->
            [];
        FileSyntaxTree ->
            lists:sort(fold_in_file_syntax_tree(FileSyntaxTree, [], fun
                ({function, _Position, FoundFunction, Arity, _Clauses}, Acc) when FoundFunction =:= Function ->
                    [Arity | Acc];
                (_, Acc) ->
                    Acc
            end))
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
find_record_field(Field, [{typed_record_field, {record_field, _, {atom, {Line, Column}, Field}}, _} | _], CurrentFile) ->
    {CurrentFile, Line, Column};
find_record_field(Field, [{typed_record_field, {record_field, _, {atom, {Line, Column}, Field}, _}, _} | _], CurrentFile) ->
    {CurrentFile, Line, Column};
find_record_field(Field, [{record_field, _, {atom, {Line, Column}, Field}} | _], CurrentFile) ->
    {CurrentFile, Line, Column};
find_record_field(Field, [{record_field, _, {atom, {Line, Column}, Field}, _} | _], CurrentFile) ->
    {CurrentFile, Line, Column};
find_record_field(Field, [_ | Tail], CurrentFile) ->
    find_record_field(Field, Tail, CurrentFile).
