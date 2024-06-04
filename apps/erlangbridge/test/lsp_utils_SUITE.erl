-module(lsp_utils_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

% Specify a list of all unit test functions
all() -> [do_is_path_excluded].

% required, but can just return Config. this is a suite level setup function.
init_per_suite(Config) ->
    %tprof:start(#{type => call_memory}),
    %tprof:enable_trace(all), tprof:set_pattern('_', '_' , '_'),
    % do custom per suite setup here
    StartResult = application:start(vscode_lsp, permanent),
    ?assertEqual(ok, StartResult),
    % to intercept traces, set to true
    ErlangSection = #{verbose => false},
    gen_lsp_config_server:update_config(
        erlang,
        ErlangSection
    ),
    Config.

% required, but can just return Config. this is a suite level tear down function.
end_per_suite(Config) ->
    % do custom per suite cleanup here
    application:stop(vscode_lsp),
    %tprof:disable_trace(all), Sample = tprof:collect(),
    %Inspected = tprof:inspect(Sample, process, measurement), Shell = maps:get(self(), Inspected),
    %tprof:format(Shell),
    Config.

otp_files() ->
    % files list extracted from otp/erts/test/erlc_SUITE_data/src/
    % returned by file:list_dir(...)
    [
        "otp/erts/test/erlc_SUITE_data/src/f_include_1.erl",
        "otp/erts/test/erlc_SUITE_data/src/erl_test_bad.erl",
        "otp/erts/test/erlc_SUITE_data/src/start_bad.script",
        "otp/erts/test/erlc_SUITE_data/src/yecc_test_bad.yrl",
        "otp/erts/test/erlc_SUITE_data/src/start_ok.script",
        "otp/erts/test/erlc_SUITE_data/src/macro_enabled.hrl",
        "otp/erts/test/erlc_SUITE_data/src/GOOD-MIB.mib",
        [11,116,112,47,101,114,116,115,47,
                  116,101,115,116,47,101,114,108,99,95,83,85,73,84,69,95,100,
                  97,116,97,47,115,114,99,47,128512,47,101,114,108,95,116,101,
                  115,116,95,117,110,105,99,111,100,101,46,101,114,108], % unicode directory
       "otp/erts/test/erlc_SUITE_data/src/CVS/older.erl",
        "otp/erts/test/erlc_SUITE_data/src/older.beam"
    ].

otp_files_withresult() ->
    % ensure that File converted to unicode:characters_to_binary before calling is_path_excludedand it works as expected
    {
    lists:zipwith(fun(X, Y) -> {unicode:characters_to_binary(X),Y} end, otp_files(), [false,false,false,false,false,false,false,false,true,false]),
    exclude_map()
    }.   

exclude_map() ->
    % #{'**/.git' => true,'**/.svn' => true,'**/.hg' => true,
    %             '**/CVS' => true,'**/.DS_Store' => true,
    %             '**/Thumbs.db' => true}. 
    #{ ".*/CVS(/.*)?$" => true }.    

check_path_excluded({FilesAndResult, Map}) ->
    lists:foreach(fun ({F, ExpectedResult}) ->
        R = lsp_utils:is_path_excluded(F, Map),
        ?assertEqual(ExpectedResult, R)
        end, FilesAndResult).

do_is_path_excluded(_Config) ->
    check_path_excluded(otp_files_withresult()),
    ok.
