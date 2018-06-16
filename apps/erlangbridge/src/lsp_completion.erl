-module(lsp_completion).

-export([module_function/2, record/2, field/3, variable/3, atom/2, attribute/1]).

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
    Description = lsp_navigation:function_description(Module, list_to_atom(Name)),
    case Description of
        <<>> ->
            #{
                label => list_to_binary(Name),
                kind => 3 % Function
            };
        _ ->
            #{
                label => list_to_binary(Name),
                kind => 3, % Function
                documentation => #{
                    value => Description,
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

atom(File, Prefix) ->
    LocalAtoms = lists:filtermap(fun (#{label := Name} = Item) ->
        case lists:prefix(Prefix, atom_to_list(Name)) of
            true -> {true, Item};
            _ -> false
        end
    end, sets:to_list(sets:from_list(local_atoms(File)))),
    StandardModules = lists:filtermap(fun (Module) ->
        case lists:prefix(Prefix, Module) of
            true -> {true, #{
                label => list_to_binary(Module),
                kind => 9 % Module
            }};
            _ -> false
        end
    end, gen_lsp_config_server:standard_modules()),
    ProjectModules = lists:filtermap(fun (Module) ->
        case lists:prefix(Prefix, Module) of
            true -> {true, #{
                label => list_to_binary(Module),
                kind => 9 % Module
            }};
            _ -> false
        end
    end, gen_lsp_doc_server:project_modules()),
    BIFs = lists:filtermap(fun (Function) ->
        case lists:prefix(Prefix, Function) of
            true -> {true, module_function_item(erlang, Function)};
            _ -> false
        end
    end, gen_lsp_config_server:bifs()),
    LocalAtoms ++ StandardModules ++ ProjectModules ++ BIFs.

local_atoms(File) ->
    FileSyntaxTree = lsp_syntax:file_syntax_tree(File),
    AtomTypes = lists:foldl(fun (TopLevelSyntaxTree, Acc) ->
        erl_syntax_lib:fold(fun (SyntaxTree, AccS) ->
            case SyntaxTree of
                {function, Position, Name, _Arity, _Clauses} ->
                    AccS#{{Position, Name} => 3}; % Function
                {remote, _, {atom, ModulePosition, Module}, {atom, FunctionPosition, Function}} ->
                    AccS#{{ModulePosition, Module} => 0, {FunctionPosition, Function} => 0};
                {call, _, {atom, Position, Name}, _} ->
                    AccS#{{Position, Name} => 0};
                {atom, Position, Name} ->
                    AccS#{{Position, Name} => 13}; % Enum
                _ ->
                    AccS
            end
        end, Acc, TopLevelSyntaxTree)
    end, #{}, FileSyntaxTree),
    maps:fold(fun
        ({_, _Name}, 0, Acc) ->
            Acc;
        ({_, Name}, 3, Acc) -> 
            Module = list_to_atom(filename:rootname(filename:basename(File))),
            [#{label => Name, kind => 3, documentation =>
                #{value => lsp_navigation:function_description(Module, Name),  kind => <<"markdown">> }
            } | Acc];
        ({_, Name}, Type, Acc) ->
            [#{label => Name, kind => Type} | Acc]
    end, [], AtomTypes).

attribute(Prefix) ->
    Attributes = ["module", "export", "include", "include_lib", "record", "behaviour", "import",
        "compile", "vsn", "on_load", "callback", "define", "file", "type", "spec"],
    lists:filtermap(fun (Attribute) ->
        case lists:prefix(Prefix, Attribute) of
            true -> {true, #{
                label => list_to_binary(Attribute),
                kind => 11 % Unit
            }};
            _ -> false
        end
    end, Attributes).
