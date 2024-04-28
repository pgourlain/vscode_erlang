-module(lsp_rename).

-export([prepareRename/3, rename/5]).
-import(lsp_syntax, [fold_in_syntax_tree/4, find_in_syntax_tree/2]).

-include("lsp_log.hrl").

prepareRename(File, Line, Column) ->
    What = find_at(File, Line, Column),
    case What of
        {ok, Value, {Start, EndPos}, _} ->
            #{
                range => lsp_utils:client_range(Line + 1, Start, EndPos),
                placeholder => list_to_binary(Value)
            };
        undefined ->
            #{};
        _Other ->
            ?LOG("not yet implemented prepareRename: ~p", [_Other]),
            #{}
    end.

%% get function (definition or call) under cursor
find_at(File, Line, Column) ->
    CursorModule = list_to_atom(filename:rootname(filename:basename(File))),
    % return undefined if File isn't under workspace
    RootWorkspace = gen_lsp_config_server:root(),
    Result =
        case string:str(File, RootWorkspace) of
            1 ->
                lsp_syntax:fold_in_syntax_tree(
                    fun
                        ({function, {L, Start}, FnName, FnArity, _}, _CurrentFile, _Acc) when
                            L == Line, Start =< Column ->
                                find_at_result(Start, Column, CursorModule, FnName, FnArity);
                        ({'fun',{L, Start}, {function, FnName, FnArity}}, _CurrentFile, _Acc) when
                            L == Line, Start =< Column ->
                                find_at_result(Start, Column, CursorModule, FnName, FnArity);
                        ({call, {L, Start}, {atom, _, FnName}, Args}, _CurrentFile, _Acc) when
                            L == Line, Start =< Column ->
                                find_at_result(Start, Column, CursorModule, FnName, length(Args));
                        (SyntaxTree, _CurrentFile, Acc) ->
                            case element(2, SyntaxTree) of
                                {L, Start} when L == Line, Start =< Column -> SyntaxTree;
                                _ -> Acc
                            end
                    end,
                    undefined,
                    File
                );
            _ ->
                undefined
        end,
    case Result of
        {ok, S, Pos, {function, _FnModule, FnName, FnArity}} ->
            %check if is in import and check if Module is local
            ImportModuleFile = find_in_import(File, FnName, FnArity),
            case string:str(ImportModuleFile, RootWorkspace) of
                1 ->
                    ImportedModule = list_to_atom(filename:rootname(filename:basename(ImportModuleFile))),
                    {ok, S, Pos, {function, ImportedModule, FnName, FnArity}};
                _ ->
                    % file is not under root namespace, so rename is not possible
                    undefined
            end;
        _ ->
            Result
    end.

find_at_result(Start, Column, FnModule, FnName, Arity) when is_integer(Arity) ->
    EndPos = Start + length(atom_to_list(FnName)),
    if
        (EndPos =< Column) -> undefined;
        true -> {ok, atom_to_list(FnName), {Start, EndPos}, {function, FnModule, FnName, Arity}}
    end;
find_at_result(_Start, _Column, _FnModule, _FnName, _Args) ->
    undefined.



% #{
% range => lsp_utils:client_range(Line+1,1,1),
% placeholder => <<"coucou">>
% }.
% { error,
%     #{
%         code => -32700, %parseError
%         message => <<"unable to prepare rename, file is not compilable.">>
%     }
% }.
rename(_Uri, File, Line, Column, NewName) ->
    What = find_at(File, Line, Column),
    Locations =
        case What of
            {ok, _Text, _Location, {function, FnModule, FnName, Arity}} ->
                find_function_references(File, FnModule, FnName, Arity);
            _ ->
                []
        end,
    #{
        %changes => [Changes]
        documentChanges => [
            #{
                textDocument => #{
                    uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(LocFile)),
                    version => 1
                },
                edits => [
                    #{
                        range => lsp_utils:client_range(LocL, Start, End),
                        newText => lsp_utils:to_binary(NewName)
                    }
                ]
            }
         || {LocFile, LocL, Start, End} <- Locations
        ]
    }.

-spec find_function_references(File::file:filename(), FnModule::atom(), Function::atom(), 
        Arity::integer()) -> [lsp_navigation:lsp_location()].
find_function_references(File, FnModule, Function, Arity) ->
    Global = [ F || [F, _, _, _] <- gen_lsp_doc_server:get_references({function, FnModule, Function, Arity})] ++ [File],
    % de-duplicate references files
    RefFiles = lists:usort(Global),
     % if global ref is non empty, find import location in target File
    ImportFromGlobal = lists:flatten([extract_function_location_from_file(F, FnModule, Function, Arity) || F <- RefFiles]),
    % sort by line
    Result = lists:usort(fun ({_F, L, C, _E}, {_F1, L1, C1, _E1}) ->
            L < L1 orelse (L =:= L1 andalso C =< C1)
        end, ImportFromGlobal),
    Result.

extract_function_location_from_file(File, Module, Function, Arity) ->
    FileModule = list_to_atom(filename:rootname(filename:basename(File))),
    Import = lsp_syntax:find_in_syntax_tree(
    fun   
        ({attribute, {_L, _C}, import, {ImportModuleName, Imports}}, _CurrentFile) 
            when Module =:= ImportModuleName ->   
            case any_tuple(Function, Arity, Imports) of
                true ->                    
                    case gen_lsp_doc_server:get_module_file(ImportModuleName) of
                        undefined -> [];
                        TargetFile -> 
                            % process imported file
                            extract_function_location_from_file(TargetFile, Module, Function, Arity)
                    end;
                _ ->
                    undefined
            end;
        (_SyntaxTree, _CurrentFile) ->
            undefined
    end, File),
    ProcessLocalCall = Import =/= undefined orelse FileModule =:= Module,    
    FromImport = if 
        Import =:= undefined -> [];
        true -> lists:flatten(Import)
    end,    
    FromImport ++ 
    lsp_syntax:fold_in_syntax_tree(fun
        ({function, {_L, _Start}, FnName, FnArity, Clauses}, _CurrentFile, Acc) when
            FnName =:= Function, FnArity =:= Arity ->
            Elts = [
                {File, ClauseLine, ClauseColumn, ClauseColumn + length(atom_to_list(FnName))}
                || {clause, {ClauseLine, ClauseColumn}, _, _, _} <- Clauses
            ],
            Elts ++ Acc;
        ({attribute, {L, C}, spec, {{FnName, FnArity}, _}}, _CurrentFile, Acc) when
            FnName =:= Function, FnArity =:= Arity ->
            Elt = {File, L, 5 + C, 5 + C + length(atom_to_list(FnName))},
            [Elt | Acc];
        ({attribute, {L, C}, export, Exports}, _CurrentFile, Acc) ->
            case any_tuple(Function, Arity, Exports)  of
                true -> parse_location_at(File, L, C, Function, Arity) ++ Acc;
                _ -> Acc
            end;       
        ({attribute, {L, C}, import, {ImportModuleName, Imports}}, _CurrentFile, Acc) 
                when Module =:= ImportModuleName ->  
            case any_tuple(Function, Arity, Imports) of
                true -> parse_location_at(_CurrentFile, L, C, Function, Arity) ++ Acc;
                _ -> Acc
            end;
        ({call, {_, _}, {remote, {_, _}, {atom, {ModuleLine, _}, FnModule}, {atom, {_L, Start}, FnName}}, Args}, _CurrentFile, Acc)
                when FnName =:= Function, length(Args) =:= Arity, FnModule =:= Module ->
            End = Start + length(atom_to_list(Function)),
            [{File, ModuleLine, Start, End} | Acc];
        ({'fun', {_, _}, {function, {atom, {ModuleLine, _}, FnModule}, {atom, {_, Start}, FnName}, {integer, {_, End}, FnArity}}}, _CurrentFile, Acc) 
                when FnName =:= Function, FnArity =:= Arity, FnModule =:= Module ->
            [{File, ModuleLine, Start, End-1} | Acc];
        ({'fun',{L, Start}, {function, FnName, FnArity}}, _CurrentFile, Acc) 
                when FnName =:= Function, FnArity =:= Arity ->
            End = Start + length(atom_to_list(Function)),
            [{File, L, Start+4, End+4} | Acc];        
        ({call, {_, _}, {atom, {ModuleLine, Start}, FnName}, Args}, _CurrentFile, Acc) 
                when ProcessLocalCall, FnName =:= Function, length(Args) =:= Arity ->
            % local call only if function is imported
            End = Start + length(atom_to_list(Function)),
            [{File, ModuleLine, Start, End} | Acc];              
        (_, _, Acc) ->
            Acc
    end, [], File).


any_tuple(Function, Arity, List) ->
    lists:any(
        fun({FnName, FnArity}) -> FnName =:= Function andalso FnArity =:= Arity end,
        List
    ).

parse_location_at(File, L, _C, Function, Arity) ->
    ExportLines = get_file_content(File, L, <<".">>),
    case erl_scan:string(ExportLines, {1, 1}) of
        {ok, Tokens, _} ->
            case location_from_exports_tokens(multi_line_tokens(Tokens,{1,0, undefined}), {Function, Arity}, []) of
                {ok, {Ls, Cs}} -> [{File, L + Ls - 1, Cs, Cs + length(atom_to_list(Function))}];
                _ -> []
            end;
        _ ->
            []
    end.

%% recalculate lines number in tokens with \n
multi_line_tokens([{atom,{_L,C},n}|Tail], {CurLine, LastColumn, escape}) ->
    [{atom,{CurLine,C-LastColumn},n}| multi_line_tokens(Tail, {CurLine+1, C, newline})];
multi_line_tokens([{'\\',{_L,C}}|Tail], {CurLine, LastColumn, _}) ->
    [{'\\',{CurLine,C-LastColumn}}| multi_line_tokens(Tail, {CurLine, LastColumn, escape})];
multi_line_tokens([{Value,{_L,C}}|Tail], {CurLine, LastColumn,_}) ->
    [{Value,{CurLine,C-LastColumn}}| multi_line_tokens(Tail, {CurLine, LastColumn, undefined})];
multi_line_tokens([{Type,{_L,C}, Value}|Tail], {CurLine, LastColumn,_}) ->
       [{Type,{CurLine,C-LastColumn}, Value}| multi_line_tokens(Tail, {CurLine, LastColumn, undefined})];
multi_line_tokens([], {_, _,_}) ->
    [].

% tokens can be import or export tokens
location_from_exports_tokens([{atom, {L, C}, FnName} | Tail], {Function, _Arity} = Options, _Acc) when
    FnName =:= Function ->
    location_from_exports_tokens(Tail, Options, {L, C});
location_from_exports_tokens([{integer, _, FnArity} | _Tail], {_Function, Arity}, {_, _} = Acc) when
    FnArity =:= Arity ->
    {ok, Acc};
location_from_exports_tokens([_H | Tail], Options, Acc) ->
    location_from_exports_tokens(Tail, Options, Acc);
location_from_exports_tokens([], _, Acc) ->
    Acc.

%% Get the contents of a file from a specific line to a specific character.
%% @param File The path to the file.
%% @param Line The line to start from (1-based).
%% @param Char The character at end.
get_file_content(File, Line, Char) ->
    %% Read the file into a binary.
    Contents = get_file_content(File),

    %% Convert the binary to a list of lines.
    Lines = binary:split(Contents, <<"\n">>, [global]),
    Startline =
        if
            (Line > 0) -> Line - 1;
            true -> line
        end,
    {_, Rest} = lists:split(Startline, Lines),
    case get_string_until(Rest, Char, []) of
        {error, _} -> "";
        L -> lsp_utils:to_string(binary:list_to_bin(L))
    end.


get_file_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            case file:read_file(File) of
            {ok, Binary} -> Binary;               
            _Err -> <<"">>
            end;
        _Content -> _Content
    end.

get_string_until([Line | Lines], Char, Acc) ->
    case binary:match(Line, Char, []) of
        nomatch ->
            [(<<Line/binary,"\\n"/utf8>>) | Acc] ++ get_string_until(Lines, Char, Acc);
        {Pos, _} ->
            %% Extract the characters from the beginning of the line to the specified character.
            case binary_part(Line, 0, Pos + 1) of
                {error, _Reason} ->
                    %% The character is out of range.
                    {error, "Character out of range."};
                Content ->
                    %% Return the contents.
                    Acc ++ [Content]
            end
    end;
get_string_until([], _Char, Acc) ->
    Acc.

%% Get Module FileName of function
find_in_import(File, Function, Arity) ->
    case
        lsp_syntax:find_in_syntax_tree(
            fun
                ({attribute, _, import, {Module, Imports}}, _CurrentFile) ->
                    IsThisImported = lists:any(
                        fun({FnName, FnArity}) -> FnName =:= Function andalso FnArity =:= Arity end,
                        Imports
                    ),
                    case IsThisImported of
                        true -> gen_lsp_doc_server:get_module_file(Module);
                        _ -> undefined
                    end;
                (_, _) ->
                    undefined
            end,
            File
        )
    of
        undefined -> File;
        _Other -> _Other
    end.
