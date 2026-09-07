-module(lsp_completion).

-export([disable_completion/0, module_function/2, record/2, field/3, variable/3, atom/2, attribute/1, macro/2]).
-export([resolve_item/1]).

disable_completion() ->
    [#{
        label => <<>>
    }].

module_function(Module, Prefix) ->
    %% get_module_file/1 returns `undefined` for a module the project can't
    %% resolve at all (e.g. a typo, or an atom that isn't actually a
    %% module) - passing that straight to get_syntax_tree/1 would try to
    %% parse the atom `undefined` itself as a file path and crash inside
    %% epp:parse_file/2.
    ExportsResult = case gen_lsp_doc_server:get_module_file(Module) of
        undefined ->
            standard_module_exports(Module);
        File ->
            case gen_lsp_doc_server:get_syntax_tree(File) of
                undefined ->
                    standard_module_exports(Module);
                SyntaxTree ->
                    syntax_tree_exports(SyntaxTree)
            end
    end,
    case ExportsResult of
        {ok, Exports} ->
            NamesOnly = [atom_to_list(element(1, Export)) || Export <- Exports],
            Unique = sets:to_list(sets:from_list(NamesOnly)),
            lists:filtermap(fun (Name) ->
                case lists:prefix(Prefix, Name) of
                    true -> {true, module_function_item(Module, Name, first_arity_for(Name, Exports))};
                    _ -> false
                end
            end, Unique);
        {error, _Error} ->
            []
    end.

first_arity_for(Name, Exports) ->
    NameAtom = list_to_atom(Name),
    case lists:keyfind(NameAtom, 1, Exports) of
        {NameAtom, Arity} -> Arity;
        false -> undefined
    end.

%% @doc task 5.6: doc lookup (lsp_navigation:function_description/2,
%% which for a stdlib/OTP module can mean an EEP-48 doc render) used to
%% happen here, eagerly, for every single candidate on every keystroke -
%% now deferred to completionItem/resolve (see resolve_item/1), and only
%% for whichever one item the user has actually highlighted. `data`
%% carries what resolve needs to redo the lookup later; it round-trips
%% through the client verbatim, same as everywhere else in this codebase
%% that stashes atoms in an item's own `data` field (they come back as
%% binaries).
%%
%% Also adds snippet support (insertTextFormat => 2): a real arity - only
%% available here, not for the BIF/local-atom paths below, which never
%% learn one - becomes a placeholder argument list the editor can tab
%% through, e.g. `foo(${1:Arg1}, ${2:Arg2})$0`.
module_function_item(Module, Name, undefined) ->
    #{
        label => list_to_binary(Name),
        kind => 3, % Function
        data => #{module => Module, function => list_to_atom(Name)}
    };
module_function_item(Module, Name, Arity) ->
    #{
        label => list_to_binary(Name),
        kind => 3, % Function
        insertText => snippet_text(Name, Arity),
        insertTextFormat => 2, % Snippet
        data => #{module => Module, function => list_to_atom(Name)}
    }.

snippet_text(Name, 0) ->
    iolist_to_binary(io_lib:format("~s()$0", [Name]));
snippet_text(Name, Arity) ->
    Placeholders = [iolist_to_binary(io_lib:format("${~p:Arg~p}", [N, N])) || N <- lists:seq(1, Arity)],
    iolist_to_binary(io_lib:format("~s(~s)$0", [Name, lists:join(<<", ">>, Placeholders)])).

%% @doc `completionItem/resolve`. Only an item this module itself gave a
%% `module`/`function` `data` (a function completion, see
%% module_function_item/3 above and the function branch of local_atoms/1
%% below) has anything to fill in - every other kind (record/field/
%% variable/attribute/macro) already carries everything it needs.
resolve_item(#{data := #{module := ModuleData, function := FunctionData}} = Item) ->
    Module = to_atom(ModuleData),
    Function = to_atom(FunctionData),
    case lsp_navigation:function_description(Module, Function) of
        <<>> ->
            maps:remove(data, Item);
        Description ->
            (maps:remove(data, Item))#{documentation => #{value => Description, kind => <<"markdown">>}}
    end;
resolve_item(Item) ->
    Item.

to_atom(Value) when is_atom(Value) -> Value;
to_atom(Value) when is_binary(Value) -> binary_to_atom(Value, utf8).

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
    end, gen_lsp_doc_server:get_syntax_tree(File)).

field(File, Record, Prefix) ->
    lists:filtermap(fun (Field) ->
        case lists:prefix(Prefix, atom_to_list(Field)) of
            true -> {true, #{label => list_to_binary(atom_to_list(Field)), kind => 5}};
            _ -> false
        end
    end, lsp_navigation:record_fields(File, Record)).

variable(File, Line, Prefix) ->
    FileSyntaxTree = gen_lsp_doc_server:get_syntax_tree(File),
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
            true -> {true, module_function_item(erlang, Function, undefined)};
            _ -> false
        end
    end, gen_lsp_config_server:bifs()),
    LocalAtoms ++ StandardModules ++ ProjectModules ++ BIFs.

local_atoms(File) ->
    FileSyntaxTree = gen_lsp_doc_server:get_syntax_tree(File),
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
            [#{label => Name, kind => 3, data => #{module => Module, function => Name}} | Acc];
        ({_, Name}, Type, Acc) ->
            [#{label => Name, kind => Type} | Acc]
    end, [], AtomTypes).

%% @doc task 5.6: `?` as a trigger character. -define is expanded away by
%% epp before the AST is built (same discovery as lsp_semantic_tokens.erl
%% and lsp_workspace_symbol.erl), so macro names are found the same way:
%% a plain per-line regex over the file's own text, not the AST.
macro(File, Prefix) ->
    Content = read_content(File),
    Lines = binary:split(Content, <<"\n">>, [global]),
    PrefixBin = list_to_binary(Prefix),
    lists:filtermap(fun (Line) ->
        case re:run(Line, <<"-define\\(\\s*([A-Za-z_][A-Za-z0-9_]*)">>, [{capture, [1], binary}]) of
            {match, [Name]} ->
                case binary:longest_common_prefix([Name, PrefixBin]) =:= byte_size(PrefixBin) of
                    true -> {true, #{label => Name, kind => 14}}; % Constant
                    false -> false
                end;
            nomatch ->
                false
        end
    end, Lines).

read_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, Bin} = file:read_file(File),
            Bin;
        Bin ->
            Bin
    end.

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
