-module(lsp_hierarchy).

-export([prepare_call_hierarchy/3, incoming_calls/1, outgoing_calls/1]).
-export([prepare_type_hierarchy/3, supertypes/1, subtypes/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 4.5: call hierarchy                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Whatever function is at (or is being called from) the cursor -
%% find_at/3 already resolves both a definition clause and a call site to
%% the same {function, Module, Function, Arity} shape, so prepare doesn't
%% need to special-case either.
prepare_call_hierarchy(File, Line, Column) ->
    case lsp_navigation:find_at(File, Line, Column) of
        {{_, {function, Module, Function, Arity}}, _} ->
            case item_for(Module, Function, Arity) of
                undefined -> [];
                Item -> [Item]
            end;
        _ ->
            []
    end.

%% @doc Every call site across the project (gen_lsp_doc_server's own
%% references cache, task 0-something's project-wide index, plus this
%% file's own local/unqualified call sites) grouped by whichever function
%% *contains* that call site (found the same way task 2.3's cursor-based
%% actions find their enclosing function).
incoming_calls(#{data := #{module := ModuleBin, function := FunctionBin, arity := Arity}}) ->
    Module = binary_to_atom(ModuleBin, utf8),
    Function = binary_to_atom(FunctionBin, utf8),
    Global = [{F, L, C, E} || [F, L, C, E] <- gen_lsp_doc_server:get_references({function, Module, Function, Arity})],
    Local = lists:flatmap(fun (File) -> lsp_navigation:local_function_references(File, Function, Arity) end,
                           gen_lsp_doc_server:get_module_files(Module)),
    Grouped = group_by_enclosing_function(Global ++ Local),
    lists:filtermap(fun ({{EncModule, EncName, EncArity}, Ranges}) ->
        case item_for(EncModule, EncName, EncArity) of
            undefined -> false;
            Item -> {true, #{from => Item, fromRanges => [range4(R) || R <- Ranges]}}
        end
    end, maps:to_list(Grouped)).

group_by_enclosing_function(Refs) ->
    lists:foldl(fun ({File, L, C, E}, Acc) ->
        Tree = gen_lsp_doc_server:get_syntax_tree(File),
        case is_list(Tree) andalso lsp_navigation:find_function_with_line(Tree, L) of
            {function, _, EncName, EncArity, _} ->
                EncModule = list_to_atom(filename:rootname(filename:basename(File))),
                Key = {EncModule, EncName, EncArity},
                Acc#{Key => [{L, C, L, E} | maps:get(Key, Acc, [])]};
            _ ->
                Acc
        end
    end, #{}, Refs).

%% @doc Every call site *within* the target function's own clauses -
%% local calls resolve to this same module (the caller's module is known
%% externally, there is no need to re-derive it per call site); calls to
%% modules with no project file of their own (stdlib/BIF) are silently
%% dropped, since there is nowhere to point an outgoing item at.
outgoing_calls(#{data := #{module := ModuleBin, function := FunctionBin, arity := Arity}}) ->
    Module = binary_to_atom(ModuleBin, utf8),
    Function = binary_to_atom(FunctionBin, utf8),
    case function_clauses(Module, Function, Arity) of
        {ok, _File, Clauses} ->
            Calls = lists:flatmap(fun (Clause) -> collect_calls_in_clause(Clause, Module) end, Clauses),
            Grouped = group_by_target(Calls),
            lists:filtermap(fun ({{TargetModule, TargetName, TargetArity}, Ranges}) ->
                case item_for(TargetModule, TargetName, TargetArity) of
                    undefined -> false;
                    Item -> {true, #{to => Item, fromRanges => [range4(R) || R <- Ranges]}}
                end
            end, maps:to_list(Grouped));
        undefined ->
            []
    end.

function_clauses(Module, Function, Arity) ->
    find_first(fun (File) ->
        Tree = gen_lsp_doc_server:get_syntax_tree(File),
        case is_list(Tree) of
            true ->
                case [Clauses || {function, _, FName, FArity, Clauses} <- Tree,
                                  FName =:= Function, FArity =:= Arity] of
                    [Clauses | _] -> {ok, File, Clauses};
                    [] -> undefined
                end;
            false ->
                undefined
        end
    end, gen_lsp_doc_server:get_module_files(Module)).

find_first(_Fun, []) -> undefined;
find_first(Fun, [X | Rest]) ->
    case Fun(X) of
        undefined -> find_first(Fun, Rest);
        Result -> Result
    end.

collect_calls_in_clause(Clause, CallerModule) ->
    erl_syntax_lib:fold(fun
        ({call, _, {atom, {L, C}, Function}, Args}, Acc) ->
            Len = length(atom_to_list(Function)),
            [{{CallerModule, Function, length(Args)}, {L, C, L, C + Len}} | Acc];
        ({call, _, {remote, _, {atom, _, Module}, {atom, {L, C}, Function}}, Args}, Acc) ->
            Len = length(atom_to_list(Function)),
            [{{Module, Function, length(Args)}, {L, C, L, C + Len}} | Acc];
        (_, Acc) ->
            Acc
    end, [], Clause).

group_by_target(Calls) ->
    lists:foldl(fun ({Target, Range}, Acc) ->
        Acc#{Target => [Range | maps:get(Target, Acc, [])]}
    end, #{}, Calls).

%% @doc Locates Function/Arity's own definition (reusing function_clauses/3
%% - the same Module/get_module_files/1-based search outgoing_calls/1
%% already needs, rather than lsp_navigation:find_definition/3, which
%% would need a real caller File to check local imports against; passing
%% a dummy one for that just to reach its "different module" branch made
%% it try, and fail, to parse the dummy name as a file) to build the
%% CallHierarchyItem VS Code needs for both directions; `data` round-trips
%% through the client verbatim, so it comes back with binary module/
%% function names (the usual JSON atom-to-string effect) - encoded that
%% way here too, for symmetry.
item_for(Module, Function, Arity) ->
    case function_clauses(Module, Function, Arity) of
        {ok, File, [{clause, {Line, Col}, _, _, _} | _]} ->
            NameLen = length(atom_to_list(Function)),
            #{
                name => iolist_to_binary(io_lib:format("~s/~p", [Function, Arity])),
                kind => 12, %% Function
                uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
                range => lsp_utils:client_range(Line, Col, Line, Col + NameLen),
                selectionRange => lsp_utils:client_range(Line, Col, Line, Col + NameLen),
                data => #{
                    module => atom_to_binary(Module, utf8),
                    function => atom_to_binary(Function, utf8),
                    arity => Arity
                }
            };
        undefined ->
            undefined
    end.

range4({L, S, LE, CE}) -> lsp_utils:client_range(L, S, LE, CE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 4.6: type hierarchy - behaviour <-> implementors,  %%
%% built directly on task 4.4's own index                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Erlang has no separate notion of "type" for type-hierarchy
%% purposes beyond the module itself - this always resolves to the
%% current file's own module, regardless of the exact cursor position
%% within it (there is no more specific "type" to disambiguate between).
prepare_type_hierarchy(File, _Line, _Column) ->
    Module = list_to_atom(filename:rootname(filename:basename(File))),
    case type_hierarchy_item(Module, File) of
        undefined -> [];
        Item -> [Item]
    end.

supertypes(#{data := #{module := ModuleBin}}) ->
    Module = binary_to_atom(ModuleBin, utf8),
    lists:filtermap(fun (B) -> type_item_filtermap(B) end, lsp_navigation:module_behaviours(Module)).

subtypes(#{data := #{module := ModuleBin}}) ->
    Module = binary_to_atom(ModuleBin, utf8),
    lists:filtermap(fun (M) -> type_item_filtermap(M) end, lsp_navigation:behaviour_implementors(Module)).

type_item_filtermap(Module) ->
    case gen_lsp_doc_server:get_module_files(Module) of
        [File | _] ->
            case type_hierarchy_item(Module, File) of
                undefined -> false;
                Item -> {true, Item}
            end;
        [] ->
            false
    end.

type_hierarchy_item(Module, File) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    Line = case is_list(Tree) of
        true ->
            case [L || {attribute, {L, _}, module, _} <- Tree] of
                [L | _] -> L;
                [] -> 1
            end;
        false ->
            1
    end,
    #{
        name => atom_to_binary(Module, utf8),
        kind => 2, %% Module
        uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
        range => lsp_utils:client_range(Line, 1, Line, 1),
        selectionRange => lsp_utils:client_range(Line, 1, Line, 1),
        data => #{module => atom_to_binary(Module, utf8)}
    }.
