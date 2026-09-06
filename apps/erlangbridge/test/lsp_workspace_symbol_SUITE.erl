-module(lsp_workspace_symbol_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 4.1: workspace/symbol. ws_one.erl exercises every emitted kind at
%% once (module, function, record, type, macro); ws_two.erl exists purely
%% to prove a query spans every project file, not just whichever one
%% happens to be open.

all() -> [
    empty_query_returns_every_symbol_kind_across_every_file,
    query_is_a_case_insensitive_substring_match,
    query_matching_nothing_returns_no_symbols,
    resolve_is_the_identity_function
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    open_and_parse(filename:join(AppDir, "ws_one.erl")),
    open_and_parse(filename:join(AppDir, "ws_two.erl")),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

open_and_parse(File) ->
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File).

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

empty_query_returns_every_symbol_kind_across_every_file(_Config) ->
    Symbols = lsp_workspace_symbol:symbols(<<>>),
    Names = [maps:get(name, S) || S <- Symbols],
    ?assert(lists:member(<<"ws_one">>, Names)),
    ?assert(lists:member(<<"ws_two">>, Names)),
    ?assert(lists:member(<<"helper_one/0">>, Names)),
    ?assert(lists:member(<<"helper_two/0">>, Names)),
    ?assert(lists:member(<<"ws_rec">>, Names)),
    ?assert(lists:member(<<"ws_type">>, Names)),
    ?assert(lists:member(<<"WS_MACRO">>, Names)),
    ?assertEqual(2, kind_count(Symbols, 2)),   %% Module: ws_one, ws_two
    ?assertEqual(2, kind_count(Symbols, 12)),  %% Function: helper_one/0, helper_two/0
    ?assertEqual(1, kind_count(Symbols, 23)),  %% Struct: ws_rec
    ?assertEqual(1, kind_count(Symbols, 5)),   %% Class (used for type): ws_type
    ?assertEqual(1, kind_count(Symbols, 14)).  %% Constant (used for macro): WS_MACRO

%% Matches regardless of case, and matches a substring anywhere in the
%% name, not just a prefix - "elper" matches both helper_one/0 and
%% helper_two/0 but neither module name.
query_is_a_case_insensitive_substring_match(_Config) ->
    Symbols = lsp_workspace_symbol:symbols(<<"ELPER">>),
    Names = lists:sort([maps:get(name, S) || S <- Symbols]),
    ?assertEqual([<<"helper_one/0">>, <<"helper_two/0">>], Names).

query_matching_nothing_returns_no_symbols(_Config) ->
    ?assertEqual([], lsp_workspace_symbol:symbols(<<"no_such_symbol_anywhere">>)).

resolve_is_the_identity_function(_Config) ->
    Symbol = #{name => <<"placeholder">>, kind => 12, location => #{}},
    ?assertEqual(Symbol, lsp_workspace_symbol:resolve(Symbol)).

kind_count(Symbols, Kind) ->
    length([S || S <- Symbols, maps:get(kind, S) =:= Kind]).
