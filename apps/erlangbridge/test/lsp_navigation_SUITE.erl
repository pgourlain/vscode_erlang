-module(lsp_navigation_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

% Specify a list of all unit test functions
all() -> [testnavigation, test_macros,
    test_references_function_local_and_cross_file,
    test_references_variable,
    test_references_unsupported_for_record_and_macro,
    test_definition_record,
    test_definition_macro,
    test_definition_include,
    test_definition_type_is_unsupported,
    test_definition_behaviour_callback_is_unsupported,
    test_type_definition_from_spec_usage,
    test_implementation_from_behaviour_attribute,
    test_implementation_from_callback_declaration,
    test_document_highlight_variable_write_and_read,
    test_document_highlight_function_definition_and_call,
    test_document_highlight_record_usage_sites,
    test_selection_range_expands_from_statement_to_document,
    test_document_links_finds_include_target_and_comment_url
].

% required, but can just return Config. this is a suite level setup function.
init_per_suite(Config) ->
    % do custom per suite setup here
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    % to intercept traces, set to true
    ErlangSection = #{verbose => false},
    gen_lsp_config_server:update_config(erlang,
                                        ErlangSection),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(Config) ->
    % do custom per suite cleanup here
    application:stop(vscode_lsp),
    Config.

% optional, can do function level setup for all functions,
% or for individual functions by matching on TestCase.
init_per_testcase(_TestCase, Config) ->
    % do custom test case setup here
    Config.

% optional, can do function level tear down for all functions,
% or for individual functions by matching on TestCase.
end_per_testcase(_TestCase, Config) ->
    % do custom test case cleanup here
    Config.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

check_result(Result, ExpectedStart, ExpectedEnd, ExpectedModuleName) when is_tuple(Result) ->
    error_logger:info_msg("check_result: ~p ~p ~p ~p~n", [Result, ExpectedStart, ExpectedEnd, ExpectedModuleName]),
    {FilePath, Line, _StartColumn, _EndColumn} = Result,
    ?assertEqual(ExpectedStart, Line - 1),
    ?assertEqual(ExpectedEnd, Line - 1),
    BaseName = filename:basename(FilePath),
    ?assertEqual(ExpectedModuleName, BaseName);
check_result([Result], ExpectedStart, ExpectedEnd, ExpectedModuleName) when is_tuple(Result) ->
    check_result(Result, ExpectedStart, ExpectedEnd, ExpectedModuleName).

check_result(Result, ExpectedStart, ExpectedEnd) when is_tuple(Result) ->
     error_logger:info_msg("check_result/3: ~p ~p ~p~n", [Result, ExpectedStart, ExpectedEnd]),
    {_File, Line, _StartColumn, _EndColumn} = Result,

    ?assertEqual(ExpectedStart, Line - 1),
    ?assertEqual(ExpectedEnd, Line - 1),
    ok;
check_result([Result], ExpectedStart, ExpectedEnd) when is_tuple(Result) ->
    check_result(Result, ExpectedStart, ExpectedEnd).

dotestfiles(AppDir, [{FileName, LocationTests}|T]) ->
    dotestfile(filename:join(AppDir,FileName), LocationTests),
    dotestfiles(AppDir, T);
dotestfiles(_AppDir, []) ->
    ok.

dotestfile(FilePath, [{Line,Column, ExpectedLine, _ExpectedColumn, ExpectedModuleName}|T]) ->
    GoTo = lsp_navigation:definition(FilePath, Line, Column),
    ?writeConsole(GoTo),
    check_result(GoTo, ExpectedLine, ExpectedLine, ExpectedModuleName),
    dotestfile(FilePath, T);  
dotestfile(FilePath, [{Line,Column, ExpectedLine, _ExpectedColumn}|T]) ->
    GoTo = lsp_navigation:definition(FilePath, Line, Column),
    ?writeConsole(GoTo),
    check_result(GoTo, ExpectedLine, ExpectedLine),
    dotestfile(FilePath, T);  
dotestfile(FilePath, [{Line,Column, ExpectedLine}|T]) ->
    GoTo = lsp_navigation:definition(FilePath, Line,Column),
    ?writeConsole(GoTo),
    check_result(GoTo, ExpectedLine, ExpectedLine),
    dotestfile(FilePath, T);  

dotestfile(_FilePath, []) ->
    ok.

navigation_datatests() ->
    % Format : [{InputFile, [{Line, Column, ResultLine, ResultColumn, ResultModule} ,...]} ,...]
    % Line and Column are should be one index based
    % ResultLine and ResultColumn should be zero index based
    [{"main.erl",[
        {16, 10, 17, 0},
        {8, 28, 3, 6},
        {6, 37, 3, 0, "mod_test.erl"},
        {24, 18, 9, 0, "data_goods.erl"},
        {24, 24, 20, 0}
        ]
    },
    {"gen_msg_test1.erl",[
        {10, 36, 27, 0},
        {16, 32, 24, 0},
        {13, 34, 33, 0}
        ]
    },
    {"gen_msg_test2.erl",[
        {10, 34, 27, 0},
        {13, 38, 24, 0}
        ]
    }
    ].

testnavigation(Config) ->
    % write standard erlang code to test whatever you want
    % use pattern matching to specify expected return values
    AppDir = (?config(data_dir, Config)),
    % set root config, induce readline all filename from AppDir
    gen_lsp_config_server:update_config(root, AppDir),
    % add all documents from root dir into documents server
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    dotestfiles(AppDir, navigation_datatests()),
    ok.

test_macros(Config) ->
    % write standard erlang code to test whatever you want
    % use pattern matching to specify expected return values
    AppDir = (?config(data_dir, Config)),
    % set root config, induce readline all filename from AppDir
    gen_lsp_config_server:update_config(root, AppDir),
    % add all documents from root dir into documents server
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),

    % test macros
    SyntaxTree = gen_lsp_doc_server:get_dodged_syntax_tree(filename:join(AppDir,"data_goods.erl")),
    Macros = lsp_syntax:get_macros(SyntaxTree),
    ?assertEqual(true, is_list(Macros)),
    ?assertEqual([{{14,11},'DEFAULT',7}], Macros)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% task 0.11 extensions: references, plus definition on     %%
%% records / macros / includes / types / behaviour callbacks %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Fixtures: nav_ext_target.erl (record/macro/type/-callback + greet/1),
%% nav_ext_caller.erl (a cross-file, cross-module caller of greet/1),
%% nav_ext_include.hrl (a real file to navigate to via -include).

nav_ext_setup(Config) ->
    AppDir = (?config(data_dir, Config)),
    gen_lsp_config_server:update_config(root, AppDir),
    gen_lsp_doc_server:root_available(),
    gen_lsp_doc_server:config_change(),
    TargetFile = filename:join(AppDir, "nav_ext_target.erl"),
    CallerFile = filename:join(AppDir, "nav_ext_caller.erl"),
    ImplFile = filename:join(AppDir, "nav_ext_impl.erl"),
    nav_ext_open_and_parse(TargetFile),
    nav_ext_open_and_parse(CallerFile),
    nav_ext_open_and_parse(ImplFile),
    {TargetFile, CallerFile}.

nav_ext_open_and_parse(File) ->
    {ok, Content} = file:read_file(File),
    gen_lsp_doc_server:document_opened(File, Content),
    gen_lsp_doc_server:parse_document(File).

%% references/3 supports function references (local same-file calls plus
%% global cross-file ones via the project-wide references cache): greet/1
%% has no local caller in its own file, only nav_ext_caller.erl's remote
%% `nav_ext_target:greet(...)` call.
test_references_function_local_and_cross_file(Config) ->
    {TargetFile, CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "greet(Name) ->"),
    Refs = lsp_navigation:references(TargetFile, Line, Column),
    ?assertEqual(1, length(Refs)),
    [{RefFile, _RefLine, _RefStart, _RefEnd}] = Refs,
    ?assertEqual(CallerFile, RefFile).

%% Variable references are scoped to the enclosing clause: Identifier is
%% used twice in use_record/1 (the parameter, and inside the record).
test_references_variable(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "Identifier}"),
    Refs = lsp_navigation:references(TargetFile, Line, Column),
    ?assertEqual(2, length(Refs)),
    ?assert(lists:all(fun ({RefFile, _, _, _}) -> RefFile =:= TargetFile end, Refs)).

%% CHARACTERIZATION: references/3's own case statement only ever matches a
%% {function,...} or {variable,...} find_at/3 result (lsp_navigation.erl:
%% 33-48) - a record name or a macro use both fall through its `_ -> []`
%% clause, even though find_at/3 itself recognizes both as reference kinds
%% (used by definition/3, see test_definition_record/test_definition_macro
%% below). "Find all references" on a record or macro is a silent no-op.
test_references_unsupported_for_record_and_macro(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {RecordLine, RecordColumn} = nav_ext_position_of(TargetContent, "#item{identifier"),
    ?assertEqual([], lsp_navigation:references(TargetFile, RecordLine, RecordColumn)),
    {MacroLine, MacroColumn} = nav_ext_position_of(TargetContent, "?GREETING"),
    ?assertEqual([], lsp_navigation:references(TargetFile, MacroLine, MacroColumn)).

test_definition_record(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "#item{identifier"),
    [{DefFile, DefLine, _, _}] = lsp_navigation:definition(TargetFile, Line, Column),
    ?assertEqual(TargetFile, DefFile),
    {DeclLine, _} = nav_ext_position_of(TargetContent, "-record(item"),
    %% CHARACTERIZATION: for a record (unlike function/macro/variable),
    %% the returned "Line" is itself a {Line, Column} tuple, not a plain
    %% integer - find_definition_in_file/4's record clause destructures
    %% the dodged tree's {attr, Line, _, _} node and forwards that inner
    %% tuple as-is (lsp_navigation.erl:686-690).
    ?assertMatch({DeclLine, _}, DefLine).

test_definition_macro(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "?GREETING"),
    [{DefFile, DefLine, _, _}] = lsp_navigation:definition(TargetFile, Line, Column),
    ?assertEqual(TargetFile, DefFile),
    {DeclLine, _} = nav_ext_position_of(TargetContent, "-define(GREETING"),
    ?assertEqual(DeclLine, DefLine).

%% -include("nav_ext_include.hrl") navigates to the included file itself
%% (position {1,1,1}, not any specific form inside it).
test_definition_include(Config) ->
    {_TargetFile, CallerFile} = nav_ext_setup(Config),
    {ok, CallerContent} = file:read_file(CallerFile),
    {Line, Column} = nav_ext_position_of(CallerContent, "-include(\"nav_ext_include.hrl\")"),
    Definitions = lsp_navigation:definition(CallerFile, Line, Column),
    ?assertEqual(1, length(Definitions)),
    [{DefFile, 1, 1, 1}] = Definitions,
    ?assertEqual(<<"nav_ext_include.hrl">>, filename:basename(DefFile)).

%% CHARACTERIZATION: find_at/3 has no reference kind at all for a -type
%% usage (unlike record/field/macro, which all have dedicated clauses -
%% lsp_navigation.erl:227-446), so "go to definition" on item_id() in
%% use_type/1's own -spec finds nothing, even though the type is defined
%% two lines above in the very same file.
test_definition_type_is_unsupported(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "item_id()) -> item_id"),
    ?assertEqual([], lsp_navigation:definition(TargetFile, Line, Column)).

%% CHARACTERIZATION: a -callback declaration is an {attribute,_,callback,_}
%% form, which find_at/3's case statement never matches either (it only
%% recognizes {function,...} definitions, never -callback specs) - clicking
%% directly on "handle" in -callback handle(term()) -> term(). finds
%% nothing, unlike clicking a real function's own clause head.
test_definition_behaviour_callback_is_unsupported(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "-callback handle"),
    ?assertEqual([], lsp_navigation:definition(TargetFile, Line, Column)).

%% task 4.3: unlike plain definition/3 (see test_definition_type_is_
%% unsupported above), type_definition/3 has its own, separate walk for
%% exactly this - a click on item_id() inside use_type/1's own -spec
%% resolves to -type item_id() :: pos_integer(). two lines above.
test_type_definition_from_spec_usage(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "item_id()) -> item_id"),
    [{DefFile, DefLine, 1, 1}] = lsp_navigation:type_definition(TargetFile, Line, Column),
    ?assertEqual(TargetFile, DefFile),
    {DeclLine, _} = nav_ext_position_of(TargetContent, "-type item_id()"),
    ?assertEqual(DeclLine, DefLine).

%% task 4.4, forward direction: a click anywhere on nav_ext_impl.erl's own
%% -behaviour(nav_ext_target) line jumps to nav_ext_target's file.
test_implementation_from_behaviour_attribute(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    AppDir = ?config(data_dir, Config),
    ImplFile = filename:join(AppDir, "nav_ext_impl.erl"),
    {ok, ImplContent} = file:read_file(ImplFile),
    {Line, Column} = nav_ext_position_of(ImplContent, "-behaviour(nav_ext_target)"),
    ?assertEqual([{TargetFile, 1, 1, 1}], lsp_navigation:implementation(ImplFile, Line, Column)).

%% task 4.4, reverse direction: a click on -callback handle(term()) -> ...
%% in nav_ext_target.erl finds nav_ext_impl.erl's own handle/1 - the
%% concrete implementation, not just "some file with a matching
%% -behaviour" - reusing the same cross-file function search
%% find_definition/3 already does for a real function reference.
test_implementation_from_callback_declaration(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    AppDir = ?config(data_dir, Config),
    ImplFile = filename:join(AppDir, "nav_ext_impl.erl"),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "-callback handle"),
    [{DefFile, DefLine, _, _}] = lsp_navigation:implementation(TargetFile, Line, Column),
    ?assertEqual(ImplFile, DefFile),
    {ok, ImplContent} = file:read_file(ImplFile),
    {DeclLine, _} = nav_ext_position_of(ImplContent, "handle(Msg) ->"),
    ?assertEqual(DeclLine, DefLine).

%% task 4.7: Identifier is use_record/1's own parameter (a "write" - each
%% clause is its own fresh binding site) and is referenced again inside
%% the record construction (a plain "read").
test_document_highlight_variable_write_and_read(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "Identifier)"),
    Highlights = lists:sort(lsp_navigation:document_highlights(TargetFile, Line, Column)),
    {ParamLine, ParamCol} = nav_ext_position_of(TargetContent, "Identifier)"),
    {UseLine, UseCol} = nav_ext_position_of(TargetContent, "Identifier}"),
    ?assertEqual(
        lists:sort([{3, ParamLine, ParamCol, ParamCol + 10}, {2, UseLine, UseCol, UseCol + 10}]),
        Highlights).

%% task 4.7: greet/1's own definition clause plus nav_ext_caller.erl's
%% remote call site - both files, one document-scoped call.
test_document_highlight_function_definition_and_call(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "greet(Name) ->"),
    {DefLine, DefCol} = nav_ext_position_of(TargetContent, "greet(Name) ->"),
    ?assertEqual([{1, DefLine, DefCol, DefCol + 5}], lsp_navigation:document_highlights(TargetFile, Line, Column)).

%% task 4.7: #item{...} construction in use_record/1 - the only occurrence
%% of the `item` record in this file (its own -record(item, ...)
%% declaration line is deliberately not highlighted - see
%% document_highlights/3's own characterization comment).
test_document_highlight_record_usage_sites(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "#item{identifier"),
    {UseLine, UseCol} = nav_ext_position_of(TargetContent, "item{identifier"),
    ?assertEqual([{1, UseLine, UseCol, UseCol + 4}], lsp_navigation:document_highlights(TargetFile, Line, Column)).

%% task 5.2: a chain innermost-first, each level strictly containing the
%% one before it, ending at the whole document - checked structurally
%% (not against hardcoded line numbers, which would make this fixture-
%% fragile) since the exact levels a given cursor sees depend on exactly
%% which statement/clause/function it lands in.
test_selection_range_expands_from_statement_to_document(Config) ->
    {TargetFile, _CallerFile} = nav_ext_setup(Config),
    {ok, TargetContent} = file:read_file(TargetFile),
    {Line, Column} = nav_ext_position_of(TargetContent, "#item{identifier"),
    Chain = lsp_navigation:selection_range(TargetFile, Line, Column),
    ?assert(length(Chain) >= 2),
    [{FirstStart, FirstEnd} | _] = Chain,
    ?assert(FirstStart =< Line andalso Line =< FirstEnd),
    {LastStart, LastEnd} = lists:last(Chain),
    ?assertEqual(1, LastStart),
    TotalLines = length(binary:split(TargetContent, <<"\n">>, [global])),
    ?assertEqual(TotalLines, LastEnd),
    Pairs = lists:zip(lists:droplast(Chain), tl(Chain)),
    %% each level strictly contains the previous one, AND dedup_ranges/1
    %% must never have left two consecutive identical levels.
    ?assert(lists:all(fun ({Inner, Outer}) -> Inner =/= Outer end, Pairs)),
    ?assert(lists:all(fun ({{S1, E1}, {S2, E2}}) -> S2 =< S1 andalso E1 =< E2 end, Pairs)).

%% task 5.7: nav_ext_caller.erl's own -include("nav_ext_include.hrl")
%% resolves to that real file (found the same way go-to-definition
%% already resolves it - see test_definition_include above), and the
%% http(s) URL in its trailing comment is found too.
test_document_links_finds_include_target_and_comment_url(Config) ->
    {_TargetFile, CallerFile} = nav_ext_setup(Config),
    Links = lsp_navigation:document_links(CallerFile),
    [IncludePath] = [P || {_, _, _, {file, P}} <- Links],
    ?assertEqual(<<"nav_ext_include.hrl">>, filename:basename(unicode:characters_to_binary(IncludePath))),
    [UrlTarget] = [U || {_, _, _, {url, U}} <- Links],
    ?assertEqual(<<"https://www.erlang.org/doc">>, UrlTarget).

%% 1-based {Line, Column} of the first character of Marker.
nav_ext_position_of(Content, Marker) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    nav_ext_find_position(Lines, list_to_binary(Marker), 1).

nav_ext_find_position([Line | Rest], MarkerBin, LineNo) ->
    case binary:match(Line, MarkerBin) of
        {Start, _Len} -> {LineNo, Start + 1};
        nomatch -> nav_ext_find_position(Rest, MarkerBin, LineNo + 1)
    end;
nav_ext_find_position([], _MarkerBin, _LineNo) ->
    error(marker_not_found).
