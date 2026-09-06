-module(lsp_symbols_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Drives the real, exported lsp_handlers:textDocument_documentSymbol/2 and
%% textDocument_codeLens/2 (Socket unused by either, so `undefined` stands
%% in), against one fixture whose call graph is fully known:
%%
%%   call_the_exported/0  -calls->  exported_used/0  -calls->  helper/0
%%   exported_unused/0     (never called)
%%   unused_private/0      (never called)
%%
%% so every codeLens combination (exported+referenced, exported+unreferenced,
%% private+referenced, private+unreferenced) is exercised at once.

all() -> [
    document_symbol_shape_and_kinds,
    document_symbol_omits_macros_and_exports_themselves,
    codelens_disabled_by_default_returns_nothing,
    codelens_shows_both_lenses_for_an_exported_referenced_function,
    codelens_shows_only_exported_for_an_exported_but_unreferenced_function,
    codelens_shows_reference_count_for_a_private_referenced_function,
    codelens_shows_unused_for_a_private_unreferenced_function
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    File = filename:join(AppDir, "symbols_source.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File),
    [{source_file, File} | Config].

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% textDocument/documentSymbol %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Functions: name is "Name/Arity" (binary), kind 12 (Function), range is
%% the function's real span (lsp_fun_utils:get_function_range/1).
document_symbol_shape_and_kinds(Config) ->
    Symbols = document_symbols(Config),
    ?assertMatch(
        #{name := <<"call_the_exported/0">>, kind := 12, location := #{range := #{}}},
        by_name(Symbols, <<"call_the_exported/0">>)
    ),
    ?assertMatch(#{name := <<"exported_used/0">>, kind := 12}, by_name(Symbols, <<"exported_used/0">>)),
    ?assertMatch(#{name := <<"helper/0">>, kind := 12}, by_name(Symbols, <<"helper/0">>)),
    %% CHARACTERIZATION: a record symbol's name is the raw atom (`item`),
    %% not a binary like every function symbol above, and its range is
    %% collapsed to a single point {L,1,L,1} at the -record attribute's
    %% line - not the real span of the record definition.
    ?assertMatch(#{name := item, kind := 23}, by_name(Symbols, item)),
    #{location := #{range := RecordRange}} = by_name(Symbols, item),
    ?assertEqual(maps:get(<<"start">>, RecordRange), maps:get(<<"end">>, RecordRange)),
    %% CHARACTERIZATION: same for a -type: raw atom name, kind 5 (Class in
    %% the LSP SymbolKind enum - there is no dedicated "type" kind).
    ?assertMatch(#{name := item_id, kind := 5}, by_name(Symbols, item_id)).

%% CHARACTERIZATION: symbol_info/1 only ever emits function/record/type
%% forms (lsp_navigation.erl:184-196) - macros, -export/-module/-spec
%% attributes, and everything else produce no symbol at all.
document_symbol_omits_macros_and_exports_themselves(Config) ->
    Symbols = document_symbols(Config),
    Names = [maps:get(name, S) || S <- Symbols],
    %% 5 functions + the record + the type, nothing else
    ?assertEqual(7, length(Symbols)),
    ?assertNot(lists:member(<<"module">>, Names)),
    ?assertNot(lists:member(<<"export">>, Names)).

%%%%%%%%%%%%%%%%%%%%%%%%%
%% textDocument/codeLens %%
%%%%%%%%%%%%%%%%%%%%%%%%%

%% codeLensEnabled defaults to false (gen_lsp_config_server.erl), so a
%% freshly-started server with no explicit opt-in shows no lenses at all.
codelens_disabled_by_default_returns_nothing(Config) ->
    ?assertEqual([], code_lenses(Config)).

codelens_shows_both_lenses_for_an_exported_referenced_function(Config) ->
    with_codelens_enabled(fun () ->
        Lenses = lenses_for(Config, <<"exported_used">>),
        Titles = lists:sort([title(L) || L <- Lenses]),
        ?assertEqual([<<"1 references">>, <<"exported">>], Titles),
        [ReferenceLens] = [L || L <- Lenses, title(L) =:= <<"1 references">>],
        ?assertMatch(
            #{data := #{function := exported_used, count := 1, exported := true},
              command := #{command := <<"editor.action.findReferences">>}},
            ReferenceLens
        )
    end).

%% CHARACTERIZATION: an exported function with zero in-project references
%% shows ONLY the "exported" lens - not "unused". The {false, true} branch
%% of textDocument_codeLens/2's case drops the reference-count lens
%% entirely, so a genuinely dead but exported function never gets flagged.
codelens_shows_only_exported_for_an_exported_but_unreferenced_function(Config) ->
    with_codelens_enabled(fun () ->
        Lenses = lenses_for(Config, <<"exported_unused">>),
        ?assertEqual([<<"exported">>], [title(L) || L <- Lenses])
    end).

codelens_shows_reference_count_for_a_private_referenced_function(Config) ->
    with_codelens_enabled(fun () ->
        Lenses = lenses_for(Config, <<"helper">>),
        ?assertEqual([<<"1 references">>], [title(L) || L <- Lenses]),
        ?assertMatch(
            [#{data := #{function := helper, count := 1, exported := false}}],
            Lenses
        )
    end).

codelens_shows_unused_for_a_private_unreferenced_function(Config) ->
    with_codelens_enabled(fun () ->
        Lenses = lenses_for(Config, <<"unused_private">>),
        ?assertEqual([<<"unused">>], [title(L) || L <- Lenses]),
        ?assertMatch(
            [#{data := #{function := unused_private, count := 0, exported := false}}],
            Lenses
        )
    end).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

document_symbols(Config) ->
    File = ?config(source_file, Config),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    lsp_handlers:textDocument_documentSymbol(undefined, Params).

by_name(Symbols, Name) ->
    [S] = [S || S <- Symbols, maps:get(name, S) =:= Name],
    S.

code_lenses(Config) ->
    File = ?config(source_file, Config),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File)}},
    lsp_handlers:textDocument_codeLens(undefined, Params).

%% The range covers only the function's name, so its width identifies which
%% function a lens belongs to (each name here has a distinct length).
lenses_for(Config, FunctionName) ->
    Width = byte_size(FunctionName),
    [L || L <- code_lenses(Config), lens_width(L) =:= Width].

lens_width(#{range := #{<<"start">> := #{character := S}, <<"end">> := #{character := E}}}) ->
    E - S.

title(#{command := #{title := Title}}) -> Title.

with_codelens_enabled(Fun) ->
    gen_lsp_config_server:update_config(erlang, #{verbose => false, codeLensEnabled => true}),
    try
        Fun()
    after
        gen_lsp_config_server:update_config(erlang, #{verbose => false, codeLensEnabled => false})
    end.
