-module(lsp_hover_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Calls lsp_navigation:hover_info/3 directly, the same way lsp_navigation_SUITE
%% calls lsp_navigation:definition/3 - hover_info is a sibling function of the
%% very module that suite already covers.
%%
%% CHARACTERIZATION (the main finding of this suite): hover_info/3's own case
%% statement only ever matches {{reference, {function, _, _, _}}, _} - every
%% other reference kind find_at/3 can produce (module, record, field, macro)
%% falls through to `undefined`, and there is no reference kind at all for a
%% -type/-spec usage. So today hover works only on function calls (project or
%% OTP/stdlib); hovering a macro, a record (name or field), or a type usage
%% always returns nothing, regardless of what lsp_handlers.erl's own doc
%% comment above hover_info/3 suggests should be possible.
%%
%% Also: hover_info's result is returned as a plain binary/string, not the
%% #{kind => <<"markdown">>, value => ...} MarkupContent map that comment
%% describes and that lsp_handlers:textDocument_hover/2 forwards verbatim as
%% `contents` - pinned by asserting the raw shape below.

all() -> [
    hover_on_project_function_shows_both_clause_heads,
    hover_on_otp_function_uses_eep48_docs,
    hover_on_macro_returns_nothing,
    hover_on_record_returns_nothing,
    hover_on_record_field_returns_nothing,
    hover_on_type_returns_nothing
].

init_per_suite(Config) ->
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    gen_lsp_config_server:update_config(erlang, #{verbose => false}),
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

%% Project function, called locally as go(1): both clause heads (go(0) and
%% go(N)) come back, joined, as markdown-styled **name**(args) text - not
%% just the clause that would actually match the call's argument.
hover_on_project_function_shows_both_clause_heads(Config) ->
    {File, Content} = source(Config),
    {Line, Column} = position_of(Content, "go(1)"),
    Hover = lsp_navigation:hover_info(File, Line, Column),
    ?assert(is_binary(Hover)),
    Text = binary_to_list(Hover),
    ?assert(string:str(Text, "**go**(0)") > 0),
    ?assert(string:str(Text, "**go**(N)") > 0).

%% lists:reverse/1 is not a project module, so function_description/3 falls
%% back to gen_lsp_help_server:get_help/2 (EEP-48 doc chunks for stdlib).
hover_on_otp_function_uses_eep48_docs(Config) ->
    {File, Content} = source(Config),
    {Line, Column} = position_of(Content, "reverse(List)"),
    Hover = lsp_navigation:hover_info(File, Line, Column),
    ?assert(is_binary(Hover)),
    Text = binary_to_list(Hover),
    ?assert(string:str(Text, "-spec reverse(") > 0),
    ?assert(string:str(Text, "reverse order") > 0).

hover_on_macro_returns_nothing(Config) ->
    {File, Content} = source(Config),
    {Line, Column} = position_of(Content, "?GOODS_ID"),
    ?assertEqual(undefined, lsp_navigation:hover_info(File, Line, Column)).

hover_on_record_returns_nothing(Config) ->
    {File, Content} = source(Config),
    {Line, Column} = position_of(Content, "#item{identifier"),
    ?assertEqual(undefined, lsp_navigation:hover_info(File, Line, Column)).

hover_on_record_field_returns_nothing(Config) ->
    {File, Content} = source(Config),
    {Line, Column} = position_of(Content, "identifier = Identifier"),
    ?assertEqual(undefined, lsp_navigation:hover_info(File, Line, Column)).

hover_on_type_returns_nothing(Config) ->
    {File, Content} = source(Config),
    {Line, Column} = position_of(Content, "item_id()) -> item_id"),
    ?assertEqual(undefined, lsp_navigation:hover_info(File, Line, Column)).

%%%%%%%%%%%%%
%% helpers %%
%%%%%%%%%%%%%

source(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "hover_source.erl"),
    {ok, Content} = file:read_file(File),
    {File, Content}.

%% 1-based {Line, Column} of the first character of Marker - hover_info/3
%% (unlike the LSP-facing handlers) takes 1-based positions directly.
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
