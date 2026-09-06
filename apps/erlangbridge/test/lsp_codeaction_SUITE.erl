-module(lsp_codeaction_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Task 2.1 laid the infrastructure (codeActionProvider/executeCommandProvider
%% capability flags - pinned in lsp_protocol_SUITE's golden capability map,
%% not duplicated here - dispatch, and lsp_rename:build_workspace_edit/1).
%% Task 2.2 adds the five erl_lint/epp-driven fixes themselves.
%%
%% Each fix test below drives the *real* pipeline end to end: get a genuine
%% diagnostic from lsp_syntax:validate_parsed_source_file/1, round-trip its
%% correlation_data through actual JSON encode/decode (exactly like a real
%% client echoing context.diagnostics[].data back on a codeAction request -
%% see lsp_syntax_SUITE's own wire-level test for why this matters: atoms
%% on the way out come back as binaries), then call
%% lsp_codeaction:code_actions/3 and inspect the resulting edit.

all() -> [
    unused_variable_is_prefixed_with_underscore,
    unused_function_is_added_to_export,
    unused_function_is_added_to_a_multiline_export,
    undefined_function_gets_a_stub_at_end_of_file,
    undefined_record_field_is_added_to_the_record,
    missing_include_line_is_removed,
    unmatched_diagnostic_produces_no_action,
    resolve_is_the_identity_function,
    build_workspace_edit_produces_one_document_change_per_edit,
    wire_level_handlers_dispatch_without_crashing,
    cursor_on_unexported_function_offers_export_with_no_existing_export,
    cursor_on_unexported_function_offers_export_into_existing_export,
    cursor_on_exported_function_offers_remove_from_export_first_entry,
    cursor_on_exported_function_offers_remove_from_export_middle_entry,
    cursor_on_exported_function_offers_remove_from_export_last_entry,
    cursor_on_sole_export_removes_it_leaving_an_empty_list,
    cursor_on_function_without_spec_offers_generate_spec,
    generate_spec_merges_arg_names_across_clauses,
    cursor_on_function_with_existing_spec_does_not_offer_generate_spec_again,
    behaviour_with_missing_callbacks_offers_one_bulk_stub_action,
    behaviour_with_every_callback_present_offers_no_action
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    Config.

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

init_per_testcase(_TestCase, Config) ->
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% erl_lint reports the unused binding's own position, so the fix needs no
%% lookup beyond the diagnostic itself.
unused_variable_is_prefixed_with_underscore(Config) ->
    [Action] = actions_for(Config, "unused_var.erl"),
    ?assertEqual(<<"Prefix unused variable with _">>, maps:get(title, Action)),
    ?assertEqual([{"unused_var.erl", <<"_Y">>}], edit_summaries(Config, Action)).

unused_function_is_added_to_export(Config) ->
    [Action] = actions_for(Config, "unused_fn.erl"),
    ?assertEqual(<<"Export unused/0">>, maps:get(title, Action)),
    ?assertEqual([{"unused_fn.erl", <<", unused/0">>}], edit_summaries(Config, Action)),
    apply_and_assert(Config, "unused_fn.erl", Action,
        <<"-module(unused_fn).\n-export([go/0, unused/0]).\n\ngo() ->\n    ok.\n\nunused() ->\n    ok.\n">>).

%% CHARACTERIZATION: the insertion point is the export list's real closing
%% bracket wherever it lands, so a multi-line list gets the new entry
%% spliced onto whatever line the "]" itself is on - readable single-line
%% output isn't attempted, only a syntactically correct one.
unused_function_is_added_to_a_multiline_export(Config) ->
    [Action] = actions_for(Config, "unused_fn_multiline_export.erl"),
    ?assertEqual([{"unused_fn_multiline_export.erl", <<", unused/0">>}], edit_summaries(Config, Action)),
    apply_and_assert(Config, "unused_fn_multiline_export.erl", Action,
        <<"-module(unused_fn_multiline_export).\n-export([\n    a/0,\n    b/1\n, unused/0]).\n\n"
          "a() -> ok.\nb(X) -> X.\n\nunused() ->\n    ok.\n">>).

undefined_function_gets_a_stub_at_end_of_file(Config) ->
    [Action] = actions_for(Config, "undefined_fn.erl"),
    ?assertEqual(<<"Create stub for helper/2">>, maps:get(title, Action)),
    ?assertEqual(
        [{"undefined_fn.erl", <<"\nhelper(_Arg1, _Arg2) ->\n    ok.\n">>}],
        edit_summaries(Config, Action)
    ),
    apply_and_assert(Config, "undefined_fn.erl", Action,
        <<"-module(undefined_fn).\n-export([go/0]).\n\ngo() ->\n    helper(1, 2).\n"
          "\nhelper(_Arg1, _Arg2) ->\n    ok.\n">>).

undefined_record_field_is_added_to_the_record(Config) ->
    [Action] = actions_for(Config, "undefined_field.erl"),
    ?assertEqual(<<"Add field c to record #rec">>, maps:get(title, Action)),
    ?assertEqual([{"undefined_field.erl", <<", c">>}], edit_summaries(Config, Action)),
    apply_and_assert(Config, "undefined_field.erl", Action,
        <<"-module(undefined_field).\n-record(rec, {a, b, c}).\n-export([go/0]).\n\n"
          "go() ->\n    R = #rec{a = 1, c = 2},\n    R.\n">>).

%% CHARACTERIZATION: there is no file to point the include at instead, so
%% the only generally-safe fix is removing the broken line - the whole
%% line, including its trailing newline, so no blank line is left behind.
missing_include_line_is_removed(Config) ->
    [Action] = actions_for(Config, "missing_include.erl"),
    ?assertEqual(<<"Remove include of missing file \"does_not_exist.hrl\"">>, maps:get(title, Action)),
    ?assertEqual([{"missing_include.erl", <<>>}], edit_summaries(Config, Action)),
    apply_and_assert(Config, "missing_include.erl", Action,
        <<"-module(missing_include).\n-export([go/0]).\n\ngo() ->\n    ok.\n">>).

%% A diagnostic whose correlation_data doesn't match any known fix (module/
%% messageBody combination) produces no action - not a crash.
unmatched_diagnostic_produces_no_action(Config) ->
    File = source_file(Config, "unused_fn.erl"),
    Diagnostic = round_trip(#{
        severity => 2,
        range => #{start => #{line => 0, character => 0}, 'end' => #{line => 0, character => 1}},
        message => <<"some warning this server has no fix for">>,
        source => <<"erl">>,
        data => #{module => erl_lint, messageBody => [deprecated_type, foo, bar]}
    }),
    Context = #{diagnostics => [Diagnostic], triggerKind => 1},
    ?assertEqual([], lsp_codeaction:code_actions(File, undefined, Context)).

resolve_is_the_identity_function(_Config) ->
    CodeAction = #{title => <<"placeholder">>, kind => <<"quickfix">>},
    ?assertEqual(CodeAction, lsp_codeaction:resolve(CodeAction)),
    WithData = CodeAction#{data => #{anything => <<"at all">>}},
    ?assertEqual(WithData, lsp_codeaction:resolve(WithData)).

%% Direct unit test of the builder (task 2.1 factors this out of
%% lsp_rename:rename/5 - see lsp_rename_SUITE for rename/5's own behavior,
%% unchanged by the refactor or by its later multi-line generalization).
build_workspace_edit_produces_one_document_change_per_edit(_Config) ->
    Edits = [
        {"/tmp/a.erl", 3, 4, 8, "renamed"},
        {"/tmp/a.erl", 10, 0, 4, "also_here"},
        {"/tmp/b.erl", 1, 2, 3, "elsewhere"}
    ],
    #{documentChanges := Changes} = lsp_rename:build_workspace_edit(Edits),
    %% one group per edit, never merged by file - matches rename/5's own
    %% pinned characterization (task 0.10)
    ?assertEqual(3, length(Changes)),
    ?assert(lists:all(fun (#{edits := E}) -> length(E) =:= 1 end, Changes)),
    Uris = [maps:get(uri, maps:get(textDocument, C)) || C <- Changes],
    ?assertEqual(2, length([U || U <- Uris, binary:match(U, <<"a.erl">>) =/= nomatch])),
    ?assertEqual(1, length([U || U <- Uris, binary:match(U, <<"b.erl">>) =/= nomatch])),
    NewTexts = lists:sort([maps:get(newText, hd(maps:get(edits, C))) || C <- Changes]),
    ?assertEqual([<<"also_here">>, <<"elsewhere">>, <<"renamed">>], NewTexts).

%% textDocument_codeAction/2, codeAction_resolve/2 and
%% workspace_executeCommand/2 are all exported and dispatchable (Socket is
%% unused by every one of them, so `undefined` stands in), and none of them
%% crash on a realistic request shape.
wire_level_handlers_dispatch_without_crashing(Config) ->
    File = source_file(Config, "unused_fn.erl"),
    Uri = lsp_utils:file_to_file_uri(File),
    CodeActionParams = #{
        textDocument => #{uri => Uri},
        range => #{start => #{line => 0, character => 0}, 'end' => #{line => 5, character => 0}},
        context => #{diagnostics => [], triggerKind => 1}
    },
    ?assertEqual([], lsp_handlers:textDocument_codeAction(undefined, CodeActionParams)),

    CodeAction = #{title => <<"placeholder">>, kind => <<"quickfix">>},
    ?assertEqual(CodeAction, lsp_handlers:codeAction_resolve(undefined, CodeAction)),

    %% CHARACTERIZATION: no command is registered yet (task 2.1/2.2 never
    %% needed workspace/executeCommand - every fix here is a plain
    %% WorkspaceEdit) - the handler is a fixed no-op regardless of what
    %% command/arguments the client asks to execute.
    ?assertEqual(null, lsp_handlers:workspace_executeCommand(undefined, #{command => <<"anything">>, arguments => []})).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 2.3: cursor-based export/spec actions %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Unlike task 2.2's fixes, these come from actions_for_cursor/2 - no
%% diagnostic is involved at all, only which function the cursor's Range
%% sits inside (lsp_navigation:find_function_with_line/2).

cursor_on_unexported_function_offers_export_with_no_existing_export(Config) ->
    Action = action_at(Config, "no_export.erl", {2, 0}, <<"Export go/0">>),
    ?assertEqual(<<"refactor">>, maps:get(kind, Action)),
    apply_and_assert(Config, "no_export.erl", Action,
        <<"-module(no_export).\n-export([go/0]).\n\ngo() ->\n    ok.\n">>).

cursor_on_unexported_function_offers_export_into_existing_export(Config) ->
    Action = action_at(Config, "existing_export.erl", {6, 0}, <<"Export helper/0">>),
    apply_and_assert(Config, "existing_export.erl", Action,
        <<"-module(existing_export).\n-export([go/0, helper/0]).\n\ngo() ->\n    ok.\n\nhelper() ->\n    ok.\n">>).

%% three_exports.erl: -export([a/0, b/0, c/0]).
cursor_on_exported_function_offers_remove_from_export_first_entry(Config) ->
    Action = action_at(Config, "three_exports.erl", {3, 0}, <<"Remove a/0 from export">>),
    apply_and_assert(Config, "three_exports.erl", Action,
        <<"-module(three_exports).\n-export([b/0, c/0]).\n\na() -> ok.\nb() -> ok.\nc() -> ok.\n">>).

cursor_on_exported_function_offers_remove_from_export_middle_entry(Config) ->
    Action = action_at(Config, "three_exports.erl", {4, 0}, <<"Remove b/0 from export">>),
    apply_and_assert(Config, "three_exports.erl", Action,
        <<"-module(three_exports).\n-export([a/0, c/0]).\n\na() -> ok.\nb() -> ok.\nc() -> ok.\n">>).

cursor_on_exported_function_offers_remove_from_export_last_entry(Config) ->
    Action = action_at(Config, "three_exports.erl", {5, 0}, <<"Remove c/0 from export">>),
    apply_and_assert(Config, "three_exports.erl", Action,
        <<"-module(three_exports).\n-export([a/0, b/0]).\n\na() -> ok.\nb() -> ok.\nc() -> ok.\n">>).

cursor_on_sole_export_removes_it_leaving_an_empty_list(Config) ->
    Action = action_at(Config, "sole_export.erl", {3, 0}, <<"Remove a/0 from export">>),
    apply_and_assert(Config, "sole_export.erl", Action,
        <<"-module(sole_export).\n-export([]).\n\na() -> ok.\n">>).

cursor_on_function_without_spec_offers_generate_spec(Config) ->
    Action = action_at(Config, "existing_export.erl", {6, 0}, <<"Generate -spec for helper/0">>),
    ?assertEqual(<<"source">>, maps:get(kind, Action)),
    apply_and_assert(Config, "existing_export.erl", Action,
        <<"-module(existing_export).\n-export([go/0]).\n\ngo() ->\n    ok.\n\n"
          "-spec helper() -> term().\nhelper() ->\n    ok.\n">>).

%% gen_spec.erl: go(X, _Y) -> X; go(_A, B) -> B. - the non-underscore name
%% at each argument position, picked from whichever clause has one (task
%% 2.3 reuses lsp_inlayhints:extract_function_args/1, the exact same
%% "prefer a real name over _" merge inlay hints already use).
generate_spec_merges_arg_names_across_clauses(Config) ->
    Action = action_at(Config, "gen_spec.erl", {3, 0}, <<"Generate -spec for go/2">>),
    ?assertEqual([{"gen_spec.erl", <<"-spec go(X :: term(), B :: term()) -> term().\n">>}],
                 edit_summaries(Config, Action)).

%% has_spec_already/1 (in the same fixture) already has a -spec: only the
%% export-toggle action is offered for it, never a second Generate -spec.
cursor_on_function_with_existing_spec_does_not_offer_generate_spec_again(Config) ->
    Actions = actions_at(Config, "gen_spec.erl", {9, 0}),
    Titles = [maps:get(title, A) || A <- Actions],
    ?assertNot(lists:any(fun (T) -> binary:match(T, <<"Generate -spec">>) =/= nomatch end, Titles)).

%% missing_callbacks.erl declares -behaviour(gen_server) but only defines
%% start_link/0: erl_lint reports one undefined_behaviour_func diagnostic
%% per mandatory callback still missing (init/1, handle_call/3,
%% handle_cast/2 - handle_info/2, terminate/2, code_change/3 etc. are all
%% declared optional by gen_server itself, so erl_lint never asks for
%% those). All three diagnostics get bundled into a single bulk action.
behaviour_with_missing_callbacks_offers_one_bulk_stub_action(Config) ->
    [Action] = actions_for(Config, "missing_callbacks.erl"),
    ?assertEqual(<<"Implement missing callbacks for gen_server">>, maps:get(title, Action)),
    ?assertEqual(<<"quickfix">>, maps:get(kind, Action)),
    ?assertEqual(3, length(maps:get(diagnostics, Action))),
    apply_and_assert(Config, "missing_callbacks.erl", Action,
        <<"-module(missing_callbacks).\n-behaviour(gen_server).\n-export([start_link/0]).\n\n"
          "start_link() ->\n    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).\n"
          "\nhandle_call(_Arg1, _Arg2, _Arg3) ->\n    ok.\n"
          "\nhandle_cast(_Arg1, _Arg2) ->\n    ok.\n"
          "\ninit(_Arg1) ->\n    ok.\n">>).

%% complete_callbacks.erl implements every mandatory gen_server callback
%% already, so erl_lint has nothing to warn about and no bulk action exists
%% to offer.
behaviour_with_every_callback_present_offers_no_action(Config) ->
    ?assertEqual([], actions_for(Config, "complete_callbacks.erl")).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

source_file(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    filename:join(AppDir, FileName).

%% Real diagnostic -> real (JSON-round-tripped) wire shape -> real
%% code_actions/3 call, exactly like a live client would trigger this.
actions_for(Config, FileName) ->
    File = source_file(Config, FileName),
    %% A file with no diagnostics at all (e.g. every behaviour callback
    %% already implemented) parses to a map with no errors_warnings key.
    Warnings = maps:get(errors_warnings, lsp_syntax:validate_parsed_source_file(File), []),
    Diagnostics = [round_trip(to_wire_diagnostic(W)) || W <- Warnings],
    Context = #{diagnostics => Diagnostics, triggerKind => 1},
    lsp_codeaction:code_actions(File, undefined, Context).

to_wire_diagnostic(#{type := Type, info := Info, correlation_data := CorrelationData}) ->
    Severity = case Type of <<"error">> -> 1; _ -> 2 end,
    Line0 = maps:get(line, Info) - 1,
    Char0 = maps:get(character, Info) - 1,
    #{
        severity => Severity,
        range => #{start => #{line => Line0, character => Char0}, 'end' => #{line => Line0, character => 255}},
        message => maps:get(message, Info),
        source => <<"erl">>,
        data => CorrelationData
    }.

%% Cursor-based (task 2.3) actions for a given 0-based {Line, Character}
%% position, with no diagnostics involved at all.
actions_at(Config, FileName, {Line, Character}) ->
    File = source_file(Config, FileName),
    Range = #{start => #{line => Line, character => Character}},
    Context = #{diagnostics => []},
    lsp_codeaction:code_actions(File, Range, Context).

action_at(Config, FileName, Position, Title) ->
    Actions = actions_at(Config, FileName, Position),
    case [A || A <- Actions, maps:get(title, A) =:= Title] of
        [Action] -> Action;
        [] -> ct:fail({action_not_found, Title, [maps:get(title, A) || A <- Actions]})
    end.

round_trip(Term) ->
    {ok, Json} = vscode_jsone:encode(Term),
    {ok, Decoded, _} = vscode_jsone_decode:decode(Json, [{keys, atom}]),
    Decoded.

%% [{BaseFileName, NewText}] for every edit in every documentChanges group
%% of Action's `edit` - lets a test assert file + replacement text without
%% hardcoding line/column numbers already covered by the fix's own logic.
edit_summaries(_Config, #{edit := #{documentChanges := Changes}}) ->
    [begin
        Uri = maps:get(uri, maps:get(textDocument, C)),
        File = lsp_utils:file_uri_to_file(Uri),
        [Edit] = maps:get(edits, C),
        {filename:basename(File), maps:get(newText, Edit)}
     end || C <- Changes].

%% Actually apply the fix's edit(s) to the real file content and assert the
%% resulting file matches Expected exactly - proves the edit is not just
%% "some text at some position" but the *correct* whole-file result.
apply_and_assert(Config, FileName, #{edit := #{documentChanges := Changes}}, Expected) ->
    File = source_file(Config, FileName),
    {ok, Original} = file:read_file(File),
    Result = lists:foldl(fun (Change, Content) -> apply_change(Content, Change) end, Original, Changes),
    ?assertEqual(Expected, Result).

apply_change(Content, #{edits := [#{range := Range, newText := NewText}]}) ->
    #{<<"start">> := #{line := SL, character := SC}, <<"end">> := #{line := EL, character := EC}} = Range,
    Lines = binary:split(Content, <<"\n">>, [global]),
    StartOffset = line_char_offset(Lines, SL, SC),
    EndOffset = line_char_offset(Lines, EL, EC),
    Before = binary:part(Content, 0, StartOffset),
    After = binary:part(Content, EndOffset, byte_size(Content) - EndOffset),
    <<Before/binary, NewText/binary, After/binary>>.

line_char_offset(Lines, Line, Character) ->
    {Before, [TargetLine | _]} = lists:split(Line, Lines),
    LineStart = lists:foldl(fun (L, Acc) -> Acc + byte_size(L) + 1 end, 0, Before),
    LineStart + min(Character, byte_size(TargetLine)).
