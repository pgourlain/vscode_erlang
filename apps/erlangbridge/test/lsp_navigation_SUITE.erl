-module(lsp_navigation_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-define(logMsg(S),
        begin
            ct:log(default, 50, "~w:~p", [self(), S], [])
        end).

-define(writeConsole(Fmt, Args),
        error_logger:info_msg(Msg, Args)).

-define(writeConsole(S),
        error_logger:info_msg("~p\n", [S])).

% Specify a list of all unit test functions
all() -> [testnavigation, test2].

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

parseFile(AppDir, [FileName | T]) ->
    FilePath = filename:join(AppDir, FileName),
    lsp_syntax:parse_source_file(FilePath, FilePath),
    parseFile(AppDir, T);
parseFile(_AppDir, []) -> ok.

check_result(Result, ExpectedStart, ExpectedEnd, ExpectedModuleName) ->
    #{range := #{<<"end">> := #{line := EndLine}, 
        <<"start">> := #{line := StartLine}},
      uri := FilePath} = Result,
    ?assertEqual(ExpectedStart, StartLine),
    ?assertEqual(ExpectedEnd, EndLine),
    BaseName = filename:basename(binary_to_list(FilePath)),
    ?assertEqual(ExpectedModuleName, BaseName).


check_result(Result, ExpectedStart, ExpectedEnd) ->
    #{range := #{<<"end">> := #{line := EndLine}, 
        <<"start">> := #{line := StartLine}}} = Result,

    ?assertEqual(ExpectedStart, StartLine),
    ?assertEqual(ExpectedEnd, EndLine),
    % sample result    
    % A = #{range =>
    %           #{<<"end">> => #{character => 0, line => 17},
    %             <<"start">> => #{character => 0, line => 17}},
    %       uri =>
    %           <<"file:///...../sources/vscode"
    %             "_erlang/_build/test/lib/vscode_lsp/test/lsp_n"
    %             "avigation_SUITE_data/main.erl">>},
    ok.

dotestfiles(AppDir, [{FileName, LocationTests}|T]) ->
    dotestfile(filename:join(AppDir,FileName), LocationTests),
    dotestfiles(AppDir, T);
dotestfiles(_AppDir, []) ->
    ok.

dotestfile(FilePath, [{Line,Column, ExpectedLine, _ExpectedColumn, ExpectedModuleName}|T]) ->
    GoTo = lsp_navigation:goto_definition(FilePath, Line,Column),
    ?writeConsole(GoTo),
    check_result(GoTo, ExpectedLine, ExpectedLine, ExpectedModuleName),
    dotestfile(FilePath, T);  
dotestfile(FilePath, [{Line,Column, ExpectedLine, _ExpectedColumn}|T]) ->
    GoTo = lsp_navigation:goto_definition(FilePath, Line,Column),
    ?writeConsole(GoTo),
    check_result(GoTo, ExpectedLine, ExpectedLine),
    dotestfile(FilePath, T);  
dotestfile(FilePath, [{Line,Column, ExpectedLine}|T]) ->
    GoTo = lsp_navigation:goto_definition(FilePath, Line,Column),
    ?writeConsole(GoTo),
    check_result(GoTo, ExpectedLine, ExpectedLine),
    dotestfile(FilePath, T);  

dotestfile(_FilePath, []) ->
    ok.

navigation_datatests() ->
    % Format : [{InputFile, [{Line, Column, ResultLine, ResultColumn} ,...]} ,...]
    % Line and Column are should be one index based
    % ResultLine and ResultColumn should be zero index based
    [{"main.erl",[
        {16, 10, 17, 0},
        {8, 28, 3, 6},
        {6, 37, 3, 0, "mod_test.erl"}
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
    dotestfiles(AppDir, navigation_datatests()),
    ok.

test2(_Config) -> ok.
