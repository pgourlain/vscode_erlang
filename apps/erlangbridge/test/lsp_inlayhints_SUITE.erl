-module(lsp_inlayhints_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Drives the real, exported lsp_handlers:textDocument_inlayHints/2 (Socket
%% unused, so `undefined` stands in). Unlike codeLens/documentSymbol, inlay
%% hints are not computed on demand: they are cached in gen_lsp_doc_server's
%% document_inlayhints table by parse_and_store/2 (gen_lsp_doc_server.erl:490)
%% whenever a file is (re)parsed, gated on inlayHintsEnabled at parse time
%% (lsp_navigation:full_inlayhints_info/3) - so the setting has to be on
%% *before* gen_lsp_doc_server:parse_document/1 runs, not just before the
%% handler call, or the cache is simply empty.

all() -> [
    %% must run before any other case flips inlayHintsEnabled to true
    inlay_hints_disabled_by_default_returns_nothing,
    hints_shown_for_a_local_call,
    %% CHARACTERIZATION (see task 5.8): a remote call to a non-project
    %% module (lists:reverse/1 here - true for virtually every stdlib/OTP
    %% call, which is the overwhelming majority of remote calls in real
    %% code) produces no hint at all. lsp_inlayhints.erl DOES have a
    %% dedicated remote-call clause (internal_inlayhint_analyze/3's third
    %% case), but it only resolves when the target module is itself in
    %% gen_lsp_doc_server:project_modules/0 - lists is not, so
    %% get_remote_function_content/2 returns `undefined` and the call site
    %% is dropped before generate_inlayhints/3 ever sees it.
    no_hint_for_a_remote_call_to_a_non_project_module,
    %% CHARACTERIZATION (see task 5.8, the %TODO at lsp_inlayhints.erl:28):
    %% argument labels always come from the callee's *clause* variable
    %% name, never from a -spec's named parameter. A clause head that
    %% pattern-matches with `_` gets an unusable "_: " label even though
    %% its -spec names the parameter.
    hint_label_ignores_the_spec_parameter_name
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

hints_shown_for_a_local_call(Config) ->
    Hints = hints(Config),
    ?assertEqual(<<"First: ">>, label_at(Hints, marker(Config, "add(1"))),
    ?assertEqual(<<"Second: ">>, label_at(Hints, marker(Config, "add(1, 2"))).

no_hint_for_a_remote_call_to_a_non_project_module(Config) ->
    Hints = hints(Config),
    %% no hint anywhere on the lists:reverse/1 call line at all
    {Line, _} = marker(Config, "lists:reverse([1"),
    ?assertEqual([], [H || #{position := #{line := L}} = H <- Hints, L =:= Line]).

hint_label_ignores_the_spec_parameter_name(Config) ->
    Hints = hints(Config),
    ?assertEqual(<<"_: ">>, label_at(Hints, marker(Config, "named_but_ignored(5"))).

inlay_hints_disabled_by_default_returns_nothing(Config) ->
    gen_lsp_config_server:update_config(erlang, #{verbose => false, inlayHintsEnabled => false}),
    File = source_file(Config),
    gen_lsp_doc_server:parse_document(File),
    ?assertEqual([], inlay_hints_request(File)),
    %% restore for any suite re-run / later case ordering
    gen_lsp_config_server:update_config(erlang, #{verbose => false, inlayHintsEnabled => true}),
    gen_lsp_doc_server:parse_document(File).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

source_file(Config) ->
    AppDir = ?config(data_dir, Config),
    filename:join(AppDir, "inlayhints_source.erl").

hints(Config) ->
    gen_lsp_config_server:update_config(erlang, #{verbose => false, inlayHintsEnabled => true}),
    File = source_file(Config),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File),
    inlay_hints_request(File).

inlay_hints_request(File) ->
    Params = #{
        textDocument => #{uri => lsp_utils:file_to_file_uri(File)},
        range => #{
            start => #{line => 0, character => 0},
            'end' => #{line => 9999, character => 0}
        }
    },
    lsp_handlers:textDocument_inlayHints(undefined, Params).

%% There is exactly one hint per {Line, Character} position in this fixture,
%% so the (0-based) position of a marker's first character identifies it.
label_at(Hints, {Line, Character}) ->
    [Label] = [maps:get(label, H) || #{position := #{line := L, character := C}} = H <- Hints,
                                      L =:= Line, C =:= Character],
    Label.

marker(Config, Marker) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "inlayhints_source.erl"),
    {ok, Content} = file:read_file(File),
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_marker(Lines, list_to_binary(Marker), 0).

find_marker([Line | Rest], MarkerBin, LineIdx) ->
    case binary:match(Line, MarkerBin) of
        {Start, _Len} -> {LineIdx, marker_char_column(Line, Start, MarkerBin)};
        nomatch -> find_marker(Rest, MarkerBin, LineIdx + 1)
    end;
find_marker([], _MarkerBin, _LineIdx) ->
    error(marker_not_found).

%% The literal argument sits at the end of the marker (e.g. "add(1" -> the
%% "1"), so its 0-based column is the marker's last character.
marker_char_column(_Line, Start, MarkerBin) ->
    Start + byte_size(MarkerBin) - 1.
