-module(lsp_navigation_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

% Specify a list of all unit test functions
all() -> [testnavigation, test_macros].

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
