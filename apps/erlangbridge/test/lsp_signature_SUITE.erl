-module(lsp_signature_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Drives the real, exported lsp_handlers:textDocument_signatureHelp/2
%% (Socket unused, so `undefined` stands in for it), exactly like a real
%% editor keystroke would - not lsp_signature directly, since the
%% Character-1/token-filtering logic that turns a cursor position into an
%% argument index lives in lsp_handlers:signature_from_location/4, not in
%% lsp_signature itself.

all() -> [
    signature_help_at_first_argument_with_spec,
    signature_help_at_second_argument_with_spec,
    signature_help_without_spec_falls_back_to_clause_arg_names,
    retrigger_recomputes_when_the_line_still_scans,
    non_retrigger_disables_help_when_the_line_cannot_be_scanned,
    retrigger_keeps_the_previous_result_when_the_line_cannot_be_scanned,
    lsp_signature_doc_layout_is_dead_code
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    File = filename:join(AppDir, "signature_source.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    [{source_file, File}, {source_content, Content} | Config].

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% add/2 has a -spec: the signature label is built from the spec's typed
%% parameters (spec_to_signatures/lsp_signature.erl), and the cursor right
%% after the opening paren + first arg pins activeParameter => 0.
%% CHARACTERIZATION: a 0-arity type like integer() renders without its
%% parens ("integer", not "integer()") - one_type/3 keeps only the type
%% name for {type, _, Type, []}.
signature_help_at_first_argument_with_spec(Config) ->
    Help = signature_at(Config, "add(1"),
    ?assertMatch(#{signatures := [_], activeSignature := 0, activeParameter := 0}, Help),
    #{signatures := [Signature]} = Help,
    ?assertEqual(<<"add(First :: integer, Second :: integer) -> integer">>, maps:get(label, Signature)).

%% Same call, cursor after the comma into the second argument: activeParameter => 1.
signature_help_at_second_argument_with_spec(Config) ->
    Help = signature_at(Config, "add(1, "),
    ?assertMatch(#{signatures := [_], activeSignature := 0, activeParameter := 1}, Help).

%% no_spec_fun/2 has no -spec, so function_doc_syntax only finds clauses;
%% merge_spec_clauses/map_clause_args falls back to {ArgName, any} per bare
%% variable argument.
%% CHARACTERIZATION: spectype_to_string(Name, any) still renders as
%% "Name :: any" (any is treated as a real type name, not "no type
%% annotation") - a -spec-less function's signature help looks like a
%% (slightly odd) fully-typed one instead of falling back to bare names.
signature_help_without_spec_falls_back_to_clause_arg_names(Config) ->
    Help = signature_at(Config, "no_spec_fun(1"),
    ?assertMatch(#{signatures := [_], activeParameter := 0}, Help),
    #{signatures := [Signature]} = Help,
    ?assertEqual(<<"no_spec_fun(A :: any, B :: any) -> any">>, maps:get(label, Signature)).

%% CHARACTERIZATION: isRetrigger has no effect on its own. As long as the
%% current line still scans with erl_scan, textDocument_signatureHelp/2
%% recomputes from scratch regardless of isRetrigger - the flag only
%% matters when signature_from_location/4 fails (see the two cases below).
retrigger_recomputes_when_the_line_still_scans(Config) ->
    Fresh = signature_at(Config, "add(1, "),
    Retriggered = signature_at_retrigger(Config, "add(1, ", #{}),
    ?assertEqual(Fresh, Retriggered).

%% A line that erl_scan cannot tokenize at all (unterminated string, no
%% closing quote anywhere on the line) makes signature_from_location/4
%% return `error`. Without a retrigger, that disables signature help outright.
non_retrigger_disables_help_when_the_line_cannot_be_scanned(Config) ->
    Help = unscannable_signature_at(Config, false, #{}),
    ?assertEqual([], Help).

%% Same unscannable line, but as a retrigger: the handler gives back whatever
%% activeSignatureHelp the client already had, unchanged - it does not
%% recompute and does not disable.
retrigger_keeps_the_previous_result_when_the_line_cannot_be_scanned(Config) ->
    Previous = #{signatures => [#{label => <<"previous">>}], activeSignature => 0, activeParameter => 0},
    Help = unscannable_signature_at(Config, true, Previous),
    ?assertEqual(Previous, Help).

%% CHARACTERIZATION: lsp_signature_doc_layout.erl (an edoc `module/2` layout
%% callback, like hover_doc_layout.erl's role for hover) is never referenced
%% by any other module in src/ - lsp_signature.erl renders EEP-48 signatures
%% itself via eep48_render_signature/5 instead. This is why task 1.0/0.14
%% finds it missing from vscode_lsp_entry:compile_needed_modules/0: nothing
%% ever calls it, dev-time hot-loading included.
lsp_signature_doc_layout_is_dead_code(_Config) ->
    SrcDir = filename:join([code:lib_dir(vscode_lsp), "src"]),
    OtherFiles = [F || F <- filelib:wildcard(filename:join(SrcDir, "*.erl")),
                        filename:basename(F) =/= "lsp_signature_doc_layout.erl"],
    References = [F || F <- OtherFiles,
                        {ok, Bin} <- [file:read_file(F)],
                        binary:match(Bin, <<"lsp_signature_doc_layout">>) =/= nomatch],
    ?assertEqual([], References).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

signature_at(Config, Marker) ->
    signature_help(Config, Marker, false, undefined).

signature_at_retrigger(Config, Marker, ActiveSignatureHelp) ->
    signature_help(Config, Marker, true, ActiveSignatureHelp).

signature_help(Config, Marker, IsRetrigger, ActiveSignatureHelp) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    {Line, Character} = position_after(Content, Marker),
    Params = params(File, Line, Character, IsRetrigger, ActiveSignatureHelp),
    lsp_handlers:textDocument_signatureHelp(undefined, Params).

%% Uses a throwaway, never-written-to-disk file path opened with a synthetic
%% one-line buffer that erl_scan cannot tokenize at all (an unterminated
%% string literal), to force signature_from_location/4's `error` branch
%% without touching signature_source.erl (which the other cases still need
%% to parse cleanly).
unscannable_signature_at(Config, IsRetrigger, ActiveSignatureHelp) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "scratch_unscannable.erl"),
    Buffer = <<"foo(\"unterminated">>,
    gen_lsp_doc_server:document_opened(File, Buffer),
    Params = params(File, 0, byte_size(Buffer) - 1, IsRetrigger, ActiveSignatureHelp),
    lsp_handlers:textDocument_signatureHelp(undefined, Params).

params(File, Line, Character, IsRetrigger, ActiveSignatureHelp) ->
    Context = case ActiveSignatureHelp of
        undefined -> #{isRetrigger => IsRetrigger};
        _ -> #{isRetrigger => IsRetrigger, activeSignatureHelp => ActiveSignatureHelp}
    end,
    #{
        textDocument => #{uri => lsp_utils:file_to_file_uri(File)},
        position => #{line => Line, character => Character},
        context => Context
    }.

%% 0-based {Line, Character} of the last character of Marker.
position_after(Content, Marker) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_position(Lines, list_to_binary(Marker), 0).

find_position([Line | Rest], MarkerBin, LineIdx) ->
    case binary:match(Line, MarkerBin) of
        {Start, Len} -> {LineIdx, Start + Len - 1};
        nomatch -> find_position(Rest, MarkerBin, LineIdx + 1)
    end;
find_position([], _MarkerBin, _LineIdx) ->
    error(marker_not_found).
