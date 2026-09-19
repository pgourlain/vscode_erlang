-module(lsp_rename_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Calls lsp_rename:prepareRename/3 and lsp_rename:rename/5 directly, the
%% same way lsp_navigation_SUITE calls lsp_navigation:definition/3.
%%
%% CHARACTERIZATION (the main finding of this suite): find_at/3
%% (lsp_rename.erl:24-68), which decides whether a position is renameable
%% at all, walks the whole file with lsp_syntax:fold_in_syntax_tree/3 and
%% keeps overwriting its accumulator with *any* node whose own position is
%% on the same line and at or before the cursor column - a plain fold, not
%% a "closest enclosing node wins" search. Its three specific clauses
%% (function head, `fun Name/Arity`, bare local call) only survive if
%% nothing else on that line is visited afterwards and also satisfies the
%% generic catch-all's guard. In practice that means:
%%   - a function's own clause head reliably works (nothing else shares
%%     its line in the traversal once the clause/function wrapper nodes,
%%     which share the same position, are the last things visited);
%%   - a bare call wrapped in a same-line `Var = call(...)` match does NOT
%%     work - the enclosing `match` node is visited after the call and
%%     clobbers the correct match with its own raw tuple;
%%   - a variable, a record use, a macro use, and a *qualified*
%%     `Mod:fun(...)` call (no dedicated clause at all) are all rejected.
%%
%% rename/5 itself is unaffected by find_at/3's narrowness once a rename
%% *starts*: find_function_references/4 (lsp_rename.erl:119-131) uses the
%% project-wide references cache instead, so it still reaches call sites
%% find_at/3 itself could never have been triggered from.

all() -> [
    prepare_rename_accepts_a_function_definition_site,
    prepare_rename_rejects_a_call_site_wrapped_in_a_match,
    prepare_rename_rejects_a_variable,
    prepare_rename_rejects_a_record_usage,
    prepare_rename_rejects_a_macro_usage,
    prepare_rename_rejects_a_qualified_remote_call_site,
    rename_local_function_produces_one_edit_group_per_location,
    rename_exported_function_produces_a_cross_file_workspace_edit
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
    AppDir = ?config(data_dir, Config),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    TargetFile = filename:join(AppDir, "rename_target.erl"),
    CallerFile = filename:join(AppDir, "rename_caller.erl"),
    open_and_parse(TargetFile),
    open_and_parse(CallerFile),
    [{target_file, TargetFile}, {caller_file, CallerFile} | Config].

end_per_suite(Config) ->
    application:stop(vscode_lsp),
    Config.

open_and_parse(File) ->
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% textDocument/prepareRename %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% local_helper/1's own clause head: placeholder is the function name,
%% range spans it.
prepare_rename_accepts_a_function_definition_site(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "local_helper(Name) ->"),
    ?assertEqual(
        #{placeholder => <<"local_helper">>},
        maps:without([range], lsp_rename:prepareRename(File, Line, Column))
    ).

%% CHARACTERIZATION: the same function name, at its call site inside
%% greet/1 ("Local = local_helper(Name)") is rejected - not because
%% find_at/3 lacks a clause for bare local calls (it has one), but because
%% the enclosing `Local = ...` match shares the call's line and is visited
%% afterwards, overwriting the correct match. See the module comment above.
prepare_rename_rejects_a_call_site_wrapped_in_a_match(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "local_helper(Name),"),
    ?assertEqual(#{}, lsp_rename:prepareRename(File, Line, Column)).

prepare_rename_rejects_a_variable(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "Local = local_helper"),
    ?assertEqual(#{}, lsp_rename:prepareRename(File, Line, Column)).

prepare_rename_rejects_a_record_usage(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "#item{identifier"),
    ?assertEqual(#{}, lsp_rename:prepareRename(File, Line, Column)).

prepare_rename_rejects_a_macro_usage(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "?GREETING"),
    ?assertEqual(#{}, lsp_rename:prepareRename(File, Line, Column)).

%% CHARACTERIZATION: unlike a bare local call, a *qualified* remote call
%% (`rename_target:greet(...)`) has no dedicated clause in find_at/3's case
%% statement at all, so clicking directly on "greet" there is rejected too -
%% even though that exact call site is itself a real reference that DOES
%% get updated once a rename is started from a spot find_at/3 does accept
%% (see rename_exported_function_produces_a_cross_file_workspace_edit).
prepare_rename_rejects_a_qualified_remote_call_site(Config) ->
    {File, Content} = caller(Config),
    {Line, Column} = position_of(Content, "rename_target:greet"),
    ?assertEqual(#{}, lsp_rename:prepareRename(File, Line, Column)).

%%%%%%%%%%%%%%%%%%%%%%%
%% textDocument/rename %%
%%%%%%%%%%%%%%%%%%%%%%%

%% CHARACTERIZATION: documentChanges has exactly one entry *per edit
%% location*, never merged by file - rename/5's list comprehension
%% (lsp_rename.erl:100-116) builds one #{textDocument, edits => [OneEdit]}
%% group per {File, Line, Start, End} tuple. Renaming local_helper/1 (its
%% clause head + its one call site, both in this same file) therefore
%% yields two separate documentChanges groups sharing the same uri, each
%% holding a single edit - not one group with two edits.
rename_local_function_produces_one_edit_group_per_location(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "local_helper(Name) ->"),
    Uri = lsp_utils:file_to_file_uri(File),
    #{documentChanges := Changes} = lsp_rename:rename(Uri, File, Line, Column, "renamed_helper"),
    ?assertEqual(2, length(Changes)),
    ?assert(lists:all(fun (#{edits := Edits}) -> length(Edits) =:= 1 end, Changes)),
    Uris = [maps:get(uri, maps:get(textDocument, C)) || C <- Changes],
    ?assert(lists:all(fun (U) -> binary:match(U, <<"rename_target.erl">>) =/= nomatch end, Uris)),
    AllEdits = lists:flatten([maps:get(edits, C) || C <- Changes]),
    ?assert(lists:all(fun (#{newText := T}) -> T =:= <<"renamed_helper">> end, AllEdits)).

%% Renaming greet/1 from its own definition (the only spot find_at/3
%% recognizes for it - see prepare_rename_rejects_a_qualified_remote_call_site
%% above) reaches across files: find_function_references/4 uses the
%% project-wide references cache, not find_at/3's narrower recognition, so
%% the edits cover both rename_target.erl (definition clause + the
%% -export([greet/1]) entry) and rename_caller.erl (the remote call site).
rename_exported_function_produces_a_cross_file_workspace_edit(Config) ->
    {File, Content} = target(Config),
    {Line, Column} = position_of(Content, "greet(Name) ->"),
    Uri = lsp_utils:file_to_file_uri(File),
    #{documentChanges := Changes} = lsp_rename:rename(Uri, File, Line, Column, "farewell"),
    %% definition clause + -export([greet/1]) entry + the remote call site
    ?assertEqual(3, length(Changes)),
    ?assert(lists:all(fun (#{edits := Edits}) -> length(Edits) =:= 1 end, Changes)),
    Uris = [maps:get(uri, maps:get(textDocument, C)) || C <- Changes],
    ?assertEqual(2, length([U || U <- Uris, binary:match(U, <<"rename_target.erl">>) =/= nomatch])),
    ?assertEqual(1, length([U || U <- Uris, binary:match(U, <<"rename_caller.erl">>) =/= nomatch])),
    AllEdits = lists:flatten([maps:get(edits, C) || C <- Changes]),
    ?assert(lists:all(fun (#{newText := T}) -> T =:= <<"farewell">> end, AllEdits)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

target(Config) ->
    File = ?config(target_file, Config),
    {ok, Content} = file:read_file(File),
    {File, Content}.

caller(Config) ->
    File = ?config(caller_file, Config),
    {ok, Content} = file:read_file(File),
    {File, Content}.

%% 1-based {Line, Column} of the first character of Marker - lsp_rename's
%% functions take 1-based positions directly, like lsp_navigation's do.
position_of(Content, Marker) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    find_position(Lines, list_to_binary(Marker), 1).

find_position([Line | Rest], MarkerBin, LineNo) ->
    case binary:match(Line, MarkerBin) of
        {Start, _Len} -> {LineNo, Start + 1};
        nomatch -> find_position(Rest, MarkerBin, LineNo + 1)
    end;
find_position([], _MarkerBin, _LineNo) ->
    error(marker_not_found).
