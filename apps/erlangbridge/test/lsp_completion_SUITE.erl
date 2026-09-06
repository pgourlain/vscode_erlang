-module(lsp_completion_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Drives the real, exported lsp_handlers:textDocument_completion/2 (Socket
%% is unused by this handler, so `undefined` stands in for it) so the
%% regex-based trigger dispatch in lsp_handlers:auto_complete/3 is exercised
%% exactly as a real editor keystroke would, not re-implemented here.
%%
%% Test positions are located by a marker substring rather than hardcoded
%% line/column numbers (see position_after/2): the marker only needs to be
%% a *prefix* of a real token already present in completion_source.erl, since
%% text_before_character/3 only ever sees a slice of the line up to the
%% cursor - what follows on the real line is irrelevant and lets the fixture
%% stay one valid, complete module instead of an half-typed one.

all() -> [
    completion_after_module_colon,
    completion_after_hash_pins_record_names,
    completion_after_hash_dot_pins_record_fields,
    completion_for_variable_in_scope,
    completion_after_dash_pins_attributes,
    bare_atom_prefix_completion,
    macro_prefix_is_not_macro_aware,
    completion_after_a_module_colon_for_an_unresolvable_module_does_not_crash
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
    %% synchronous project scan: root_available/config_change are casts, but
    %% they are handled by gen_lsp_doc_server strictly before any later call
    %% we make on the same process (e.g. get_module_file/1 for "mod:"
    %% completion), because gen_server drains its mailbox in arrival order.
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    File = filename:join(AppDir, "completion_source.erl"),
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    [{source_file, File}, {source_content, Content} | Config].

end_per_testcase(_TestCase, Config) ->
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% "mod:" -> module_function/2: only the two names sharing the "al" prefix
%% come back, each as kind 3 (Function).
%% CHARACTERIZATION: no completion category anywhere in lsp_completion.erl
%% ever sets `insertText` - the client always falls back to inserting the
%% raw `label`. Checked once here; the shape is the same in every other case.
completion_after_module_colon(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "completion_target:al"),
    Items = complete_at(File, Position),
    ?assertEqual(
        lists:sort([<<"alpha">>, <<"alpha_two">>]),
        lists:sort([label(I) || I <- Items])
    ),
    ?assert(lists:all(fun (I) -> kind(I) =:= 3 end, Items)),
    ?assert(lists:all(fun (I) -> not maps:is_key(insertText, I) end, Items)).

%% "#" -> record/2: only records whose name matches the prefix, kind 22 (Struct).
completion_after_hash_pins_record_names(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "#it"),
    Items = complete_at(File, Position),
    ?assertEqual([#{label => <<"item">>, kind => 22}], Items).

%% "#record.field" -> field/3: only fields matching the prefix, kind 5 (Field).
completion_after_hash_dot_pins_record_fields(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "#item.iden"),
    Items = complete_at(File, Position),
    ?assertEqual(
        lists:sort([<<"identifier">>, <<"ident_extra">>]),
        lists:sort([label(I) || I <- Items])
    ),
    ?assert(lists:all(fun (I) -> kind(I) =:= 5 end, Items)).

%% Bare uppercase prefix -> variable/3: only in-scope variables of the
%% enclosing function (go/1) matching the prefix, kind 6 (Variable).
completion_for_variable_in_scope(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "LocalVar = Identif"),
    Items = complete_at(File, Position),
    ?assertEqual([#{label => <<"Identifier">>, kind => 6}], Items).

%% "-" at the very start of the line -> attribute/1: a fixed, static list -
%% not derived from the file at all, so this needs a synthetic one-line
%% buffer instead of a marker in the real fixture (a bare "-reco" would not
%% be valid Erlang on disk).
completion_after_dash_pins_attributes(Config) ->
    File = ?config(source_file, Config),
    gen_lsp_doc_server:document_opened(File, <<"-reco">>),
    Items = complete_at(File, {0, 4}),
    ?assertEqual([#{label => <<"record">>, kind => 11}], Items).

%% Bare lowercase prefix -> atom/2: matches across every category at once
%% (local atoms from the file's own syntax tree, project modules, standard
%% modules, BIFs) with no de-duplication between them.
%% CHARACTERIZATION: "completion_target" is both a project module (used as
%% `mod:fun` elsewhere) and, on this very line, a bare atom literal - it
%% therefore comes back twice, once per category (kind 9 and kind 13).
%% CHARACTERIZATION: local_atoms/1 (kind 13 here) labels with the raw atom
%% (`completion_target`), not a binary - every other completion category
%% (module_function, record, field, variable, attribute, standard/project
%% modules, BIFs) labels with a binary. lsp_completion never normalizes this.
bare_atom_prefix_completion(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "Other = compl"),
    Items = complete_at(File, Position),
    ?assertEqual(
        lists:sort([{<<"completion_source">>, 9}, {<<"completion_target">>, 9}, {completion_target, 13}]),
        lists:sort([{label(I), kind(I)} || I <- Items])
    ).

%% CHARACTERIZATION (no dedicated macro branch): auto_complete/3 has no
%% pattern for a "?" prefix (lsp_handlers.erl / lsp_completion.erl both lack
%% one), and ?MAX_ID is an ALL_CAPS macro name, so "?MAX_" is misclassified
%% by the "variable" regex (preceding "?" satisfies its non-identifier
%% guard) instead of anything macro-aware. It is then resolved as a
%% variable-in-scope search for a variable literally named "MAX_..." in
%% go/1, which does not exist (epp already expanded ?MAX_ID away before
%% erl_lint/erl_syntax ever see it) - so a real user typing a macro
%% reference gets zero suggestions today.
macro_prefix_is_not_macro_aware(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "MacroRef = ?MAX_"),
    Items = complete_at(File, Position),
    ?assertEqual([], Items).

%% Regression: typing "mod:" for a module that doesn't resolve to any real
%% project or stdlib file used to crash the whole request. module_function/2
%% (lsp_completion.erl) passed gen_lsp_doc_server:get_module_file/1's
%% `undefined` result straight into get_syntax_tree/1, which then tried to
%% epp:parse_file/2 the atom `undefined` itself as a path and threw badarg -
%% found via real-world manual testing of task 1.4, not by this suite.
completion_after_a_module_colon_for_an_unresolvable_module_does_not_crash(Config) ->
    File = ?config(source_file, Config),
    Content = ?config(source_content, Config),
    Position = position_after(Content, "nosuchmodule:g"),
    ?assertEqual([], complete_at(File, Position)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

label(#{label := Label}) -> Label.
kind(#{kind := Kind}) -> Kind.

complete_at(File, {Line, Character}) ->
    Params = #{
        textDocument => #{uri => lsp_utils:file_to_file_uri(File)},
        position => #{line => Line, character => Character}
    },
    lsp_handlers:textDocument_completion(undefined, Params).

%% 0-based {Line, Character} such that text_before_character/3 (which takes
%% Character + 1 bytes of the line) returns exactly the text up to and
%% including the last character of Marker.
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
