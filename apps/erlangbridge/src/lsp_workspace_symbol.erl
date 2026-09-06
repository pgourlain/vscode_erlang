-module(lsp_workspace_symbol).

-export([symbols/1, resolve/1]).

%% @doc `workspace/symbol` (task 4.1). Scans every file the project scan
%% already knows about (gen_lsp_doc_server:all_project_files/0 - no
%% separate persistent index is built; this always reflects whatever is
%% currently parsed, at the cost of rescanning the whole project on every
%% query) and reuses lsp_navigation:symbol_info/1 (documentSymbol's own
%% engine) for functions/records/types, adding module declarations and
%% macro definitions on top - matching this task's own "functions,
%% records, types, macros, behaviours" list (a module's own -behaviour is
%% not a separate searchable symbol here - the module itself already is
%% one, and 4.4/4.6 handle behaviour navigation on their own terms).
%%
%% A workspace/symbol query is matched with a plain case-insensitive
%% substring test, not fuzzy scoring - VS Code re-ranks/highlights
%% matches client-side regardless of what the server returns.
-spec symbols(Query :: binary() | string()) -> [map()].
symbols(Query) ->
    QueryLower = string:lowercase(unicode:characters_to_binary(Query)),
    lists:flatmap(fun (File) -> file_symbols(File, QueryLower) end, gen_lsp_doc_server:all_project_files()).

%% @doc Every symbol already carries its full location, so there is
%% nothing to lazily fill in - identity, like lsp_codeaction:resolve/1.
resolve(Symbol) ->
    Symbol.

file_symbols(File, QueryLower) ->
    AllSymbols = module_symbols(File) ++ lsp_navigation:symbol_info(File) ++ macro_symbols(File),
    [to_symbol_information(File, Name, Kind, Range)
     || {Name, Kind, Range} <- AllSymbols, matches_query(Name, QueryLower)].

matches_query(_Name, <<>>) ->
    true;
matches_query(Name, QueryLower) ->
    NameLower = string:lowercase(name_to_binary(Name)),
    binary:match(NameLower, QueryLower) =/= nomatch.

name_to_binary(Name) when is_atom(Name) -> atom_to_binary(Name, utf8);
name_to_binary(Name) when is_binary(Name) -> Name.

%% Kind 2 = Module.
module_symbols(File) ->
    Tree = gen_lsp_doc_server:get_syntax_tree(File),
    case is_list(Tree) of
        true -> [{ModuleName, 2, {L, 1, L, 1}} || {attribute, {L, _}, module, ModuleName} <- Tree];
        false -> []
    end.

%% -define is expanded away by epp before the AST is built (same
%% discovery as lsp_semantic_tokens.erl), so it is found the same way:
%% a plain per-line regex, not the AST. Kind 14 = Constant.
macro_symbols(File) ->
    Content = read_content(File),
    Lines = binary:split(Content, <<"\n">>, [global]),
    IndexedLines = lists:zip(lists:seq(1, length(Lines)), Lines),
    lists:flatmap(fun ({LineNum, LineContent}) ->
        case re:run(LineContent, <<"-define\\(\\s*([A-Za-z_][A-Za-z0-9_]*)">>, [{capture, [1], binary}]) of
            {match, [Name]} -> [{Name, 14, {LineNum, 1, LineNum, 1}}];
            nomatch -> []
        end
    end, IndexedLines).

to_symbol_information(File, Name, Kind, {L, S, LE, CE}) ->
    #{
        name => name_to_binary(Name),
        kind => Kind,
        location => #{
            uri => lsp_utils:file_uri_to_vscode_uri(lsp_utils:file_to_file_uri(File)),
            range => lsp_utils:client_range(L, S, LE, CE)
        }
    }.

read_content(File) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            {ok, Bin} = file:read_file(File),
            Bin;
        Bin ->
            Bin
    end.
