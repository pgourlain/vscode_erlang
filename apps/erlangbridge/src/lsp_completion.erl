-module(lsp_completion).

-export([module_function/2, record/2, field/3, variable/3]).

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
            lists:filtermap(fun (Name) ->
                case lists:prefix(Prefix, Name) of
                    true -> {true, module_function_item(Module, Name)};
                    _ -> false
                end
            end, Unique);
        {error, _Error} ->
            []
    end.

module_function_item(Module, Name) ->
    Help = gen_lsp_help_server:get_help(Module, list_to_atom(Name)),
    case Help of
        undefined ->
            #{
                label => list_to_binary(Name),
                kind => 3 % Function
            };
        _ ->
            #{
                label => list_to_binary(Name),
                kind => 3, % Function
                documentation => #{
                    value => Help,
                    kind => <<"markdown">>
                }
            }
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
    lists:filtermap(fun 
        ({attribute, _, record, {Name, _}}) ->
            case lists:prefix(Prefix, atom_to_list(Name)) of
                true -> {true, #{
                    label => list_to_binary(atom_to_list(Name)),
                    kind => 22 % Struct 
                }};
                _ -> false
            end;
        (_) ->
            false
    end, lsp_syntax:file_syntax_tree(File)).

field(File, Record, Prefix) ->
    RecordTree = lsp_navigation:find_record(lsp_syntax:file_syntax_tree(File), Record),
    case RecordTree of
        {{attribute, _, record, {Record, Fields}}, _File} ->
            lists:filtermap(fun 
                ({record_field, _, {atom, _, Field}}) ->
                    case lists:prefix(Prefix, atom_to_list(Field)) of
                        true -> {true, #{
                            label => list_to_binary(atom_to_list(Field)),
                            kind => 5 % Field
                        }};
                        _ -> false
                    end;
                (_) ->
                    false
            end, Fields);
        _ ->
            []
    end.

variable(File, Line, Prefix) ->
    FileSyntaxTree = lsp_syntax:file_syntax_tree(File),
    Function = lsp_navigation:find_function_with_line(FileSyntaxTree, Line),
    case Function of
        undefined ->
            [];
        _ ->
            Names = erl_syntax_lib:fold(fun (SyntaxTree, Acc) ->
                case SyntaxTree of
                    {var, _, Name} ->
                        [Name | Acc];
                    _ ->
                        Acc
                end
            end, [], Function),
            Unique = sets:to_list(sets:from_list(Names)),
            lists:filtermap(fun (Name) ->
                case lists:prefix(Prefix, atom_to_list(Name)) of
                    true -> {true, #{
                        label => list_to_binary(atom_to_list(Name)),
                        kind => 6 % Variable
                    }};
                    _ -> false
                end
            end, Unique)
    end.
