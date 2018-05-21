-module(lsp_completion).

-export([module_function/2, record/2]).

module_function(Module, Prefix) ->
    SyntaxTreeFile = lsp_syntax:module_syntax_tree(Module),
    ExportsResult = case SyntaxTreeFile of
        undefined ->
            standard_module_exports(Module);
        {SyntaxTree, _File} ->
            syntax_tree_exports(SyntaxTree)
    end,
    case ExportsResult of
        {ok, Exports} ->
            NamesOnly = [atom_to_list(element(1, Export)) || Export <- Exports],
            Unique = sets:to_list(sets:from_list(NamesOnly)),
            #{items => lists:filtermap(fun (Name) ->
                case lists:prefix(Prefix, Name) of
                    true -> {true, list_to_binary(Name)};
                    _ -> false
                end
            end, Unique)};
        {error, Error} ->
            #{error => list_to_binary(Error)}
    end.

standard_module_exports(Module) ->
    case code:ensure_loaded(Module) of
        {module, _} -> {ok, proplists:get_value(exports, Module:module_info())};
        _ -> {error, "No such module"}
    end.

syntax_tree_exports(SyntaxTree) ->
    {ok, lists:foldl(fun
        ({attribute, _, export, Exports}, Acc) ->
            lists:append(Exports, Acc);
        (_, Acc) ->
            Acc
    end, [], SyntaxTree)}.

record(File, Prefix) ->
    SyntaxTree = lsp_syntax:file_syntax_tree(File),
    case SyntaxTree of
        undefined ->
            #{error => <<"Cannot find module">>};
        _ ->
            error_logger:info_msg("record ~p",[SyntaxTree]),
            #{items => lists:filtermap(fun 
                ({attribute, _, record, {Name, _}}) ->
                    case lists:prefix(Prefix, atom_to_list(Name)) of
                        true -> {true, list_to_binary(atom_to_list(Name))};
                        _ -> false
                    end;
                (_) ->
                    false
            end, SyntaxTree)}
    end.