-module(lsp_syntax_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Characterizes lsp_syntax:validate_parsed_source_file/1 (erl_lint diagnostics)
%% and lsp_parse:parse_config_file/2 (.src / .config term-file diagnostics)
%% at the module level, the same way lsp_navigation_SUITE calls lsp_navigation
%% directly. One extra test case goes one layer up, through the exported
%% lsp_handlers:textDocument_didOpen/2, to pin the wire-level severity mapping
%% and range/data shape that lsp_handlers:send_diagnostics/3 builds on top of
%% lsp_syntax's output (lsp_handlers.erl:472-500) - that mapping cannot be
%% observed by calling lsp_syntax alone.

all() -> [
    syntax_error_is_reported,
    unused_variable_is_reported,
    unused_function_is_reported,
    missing_include_is_reported,
    non_latin1_path_and_message_are_utf8,
    error_in_included_file_is_reported_on_the_include_line,
    module_and_header_problems_are_both_reported,
    header_problem_is_on_the_include_line_with_crlf_endings,
    header_problem_is_on_the_include_line_with_a_trailing_comment,
    bad_record_field_is_reported,
    parse_transform_from_rebar_config_is_applied,
    eunit_generator_is_not_reported_as_unused,
    valid_app_src_parses_cleanly,
    invalid_app_src_reports_the_parse_error,
    valid_rebar_config_parses_cleanly,
    invalid_rebar_config_reports_the_parse_error,
    diagnostics_pin_severity_range_and_data
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

end_per_testcase(_TestCase, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lsp_syntax:validate_parsed_source_file/1 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

syntax_error_is_reported(Config) ->
    Item = the_one_diagnostic(Config, "syntax_error.erl"),
    ?assertMatch(#{type := <<"error">>}, Item),
    #{info := Info} = Item,
    ?assertEqual(6, maps:get(line, Info)),
    assert_message_contains(Info, "syntax error").

unused_variable_is_reported(Config) ->
    Item = the_one_diagnostic(Config, "unused_variable.erl"),
    ?assertMatch(#{type := <<"warning">>}, Item),
    #{info := Info} = Item,
    ?assertEqual(5, maps:get(line, Info)),
    assert_message_contains(Info, "unused").

unused_function_is_reported(Config) ->
    Item = the_one_diagnostic(Config, "unused_function.erl"),
    ?assertMatch(#{type := <<"warning">>}, Item),
    #{info := Info} = Item,
    ?assertEqual(10, maps:get(line, Info)),
    assert_message_contains(Info, "unused").

missing_include_is_reported(Config) ->
    Item = the_one_diagnostic(Config, "missing_include.erl"),
    ?assertMatch(#{type := <<"error">>}, Item),
    #{info := Info} = Item,
    ?assertEqual(2, maps:get(line, Info)),
    assert_message_contains(Info, "does_not_exist.hrl").

%% erl_lint reports a header's problems in a group of their own, at the
%% header's positions. They used to be shown at those positions in the module
%% (a syntax error on line 1 of the header underlined line 1 of the module);
%% they go on the -include line as one diagnostic naming the header, with no
%% correlation data - a quick fix would edit the module at the header's
%% positions - and a link to each problem in the header, carrying its message
%% (the summary does not repeat it, or the hover shows it twice).
error_in_included_file_is_reported_on_the_include_line(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "include_with_error.erl"),
    Hrl = filename:join(AppDir, "broken_include.hrl"),
    #{errors_warnings := [Item]} = lsp_syntax:validate_parsed_source_file(File),
    ?assertMatch(#{type := <<"error">>, info := #{line := 2, character := 1}}, Item),
    #{info := #{message := Message}} = Item,
    ?assertEqual(<<"1 error in included file broken_include.hrl">>, Message),
    ?assertNot(maps:is_key(correlation_data, Item)),
    ?assertMatch([#{file := Hrl, line := 1, character := 17,
                    message := <<"syntax error before: ')'">>}], maps:get(related, Item)).

%% Two groups - the module's and the header's - used to fall through the
%% single-group patterns in lsp_syntax:lint/2 and drop every diagnostic.
module_and_header_problems_are_both_reported(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "include_and_module_problems.erl"),
    #{errors_warnings := Items} = lsp_syntax:validate_parsed_source_file(File),
    ?assertMatch([#{type := <<"error">>, info := #{line := 2}},
                  #{type := <<"warning">>, info := #{line := 6}}], Items).

%% epp says where the module resumes after a header, which is the line after
%% the -include only when the `.` ending it is followed by a bare newline:
%% with CRLF endings or a comment after it, it is the -include line itself -
%% and the problem used to land on the line above.
header_problem_is_on_the_include_line_with_crlf_endings(Config) ->
    assert_header_problem_on_line(Config, "include_with_error_crlf.erl", 3).

header_problem_is_on_the_include_line_with_a_trailing_comment(Config) ->
    assert_header_problem_on_line(Config, "include_with_error_comment.erl", 3).

assert_header_problem_on_line(Config, FileName, Line) ->
    File = filename:join(?config(data_dir, Config), FileName),
    #{errors_warnings := [Item]} = lsp_syntax:validate_parsed_source_file(File),
    ?assertMatch(#{info := #{line := Line}}, Item).

%% File names and messages are lists of Unicode code points: turned into
%% binaries byte by byte, a character above 255 (a module under a folder
%% named in Japanese) crashed the whole lint, and one between 128 and 255 (an
%% accented variable name) came out as Latin-1, not the UTF-8 JSON expects.
%% Written at test time so that no non-ASCII name is committed.
non_latin1_path_and_message_are_utf8(Config) ->
    Dir = filename:join(?config(priv_dir, Config), [26085, 26412]), % "日本"
    File = filename:join(Dir, "unicode_path.erl"),
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, <<"-module(unicode_path).\n-export([go/0]).\n\ngo() ->\n    Été = 1,\n    ok.\n"/utf8>>),
    #{errors_warnings := [Item]} = lsp_syntax:validate_parsed_source_file(File),
    ?assertEqual(unicode:characters_to_binary(File), maps:get(file, Item)),
    ?assertEqual(<<"variable 'Été' is unused"/utf8>>, maps:get(message, maps:get(info, Item))).

bad_record_field_is_reported(Config) ->
    Item = the_one_diagnostic(Config, "bad_record_field.erl"),
    ?assertMatch(#{type := <<"error">>}, Item),
    #{info := Info} = Item,
    ?assertEqual(6, maps:get(line, Info)),
    assert_message_contains(Info, "rec").

%% #216: a parse transform declared as `{parse_transform, M}` in rebar.config's
%% erl_opts - not via a -compile(...) attribute in the module - must still run
%% before linting. consumer.erl calls a helper/0 that only exists once
%% inject_helper:parse_transform/2 injects it (see the fixture files under
%% lsp_syntax_SUITE_data/parse_transform/).
parse_transform_from_rebar_config_is_applied(Config) ->
    AppDir = ?config(data_dir, Config),
    SubDir = filename:join(AppDir, "parse_transform"),
    gen_lsp_config_server:update_config(root, SubDir),
    File = filename:join(SubDir, "consumer.erl"),
    ?assertEqual(#{parse_result => true}, lsp_syntax:validate_parsed_source_file(File)).

%% #89: eunit exports every 0-arity function whose name ends in "_test" or
%% "_test_" (generators). Both must be filtered out of unused_function
%% warnings, not just the plain "_test" case.
eunit_generator_is_not_reported_as_unused(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "eunit_generator.erl"),
    ?assertEqual(#{parse_result => true}, lsp_syntax:validate_parsed_source_file(File)).

%% Since task 2.1, every errors_warnings item also carries a generic,
%% JSON-safe `correlation_data` (module + the raw erl_lint/erl_parse
%% message body reshaped into JSON-safe arrays/binaries) - see
%% lsp_syntax:correlation_data/1. Before 2.1 this key never existed at
%% all (lsp_handlers:send_diagnostics/3 forwarded `null` unconditionally);
%% see diagnostics_pin_severity_range_and_data below for the
%% wire-level shape this produces.
the_one_diagnostic(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, FileName),
    Result = lsp_syntax:validate_parsed_source_file(File),
    ?assertMatch(#{parse_result := true, errors_warnings := [_ | _]}, Result),
    #{errors_warnings := [Item | _]} = Result,
    ?assertEqual(lists:sort([type, file, info, correlation_data]), lists:sort(maps:keys(Item))),
    ?assertEqual(unicode:characters_to_binary(File), maps:get(file, Item)),
    ?assertEqual(lists:sort([module, messageBody]), lists:sort(maps:keys(maps:get(correlation_data, Item)))),
    #{info := Info} = Item,
    ?assertEqual(lists:sort([line, character, message]), lists:sort(maps:keys(Info))),
    Item.

assert_message_contains(Info, Needle) ->
    Message = binary_to_list(maps:get(message, Info)),
    ?assert(string:str(string:to_lower(Message), string:to_lower(Needle)) > 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lsp_parse:parse_config_file/2 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_app_src_parses_cleanly(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "valid.app.src"),
    ?assertEqual(#{parse_result => true}, lsp_parse:parse_config_file(File, File)).

invalid_app_src_reports_the_parse_error(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "invalid.app.src"),
    Result = lsp_parse:parse_config_file(File, File),
    ?assertMatch(#{parse_result := true, errors_warnings := [_]}, Result),
    #{errors_warnings := [Item]} = Result,
    ?assertEqual(#{type => <<"error">>, file => list_to_binary(File)}, maps:without([info], Item)),
    ?assert(maps:is_key(line, maps:get(info, Item))),
    ?assert(maps:is_key(message, maps:get(info, Item))).

valid_rebar_config_parses_cleanly(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "valid_rebar.config"),
    ?assertEqual(#{parse_result => true}, lsp_parse:parse_config_file(File, File)).

invalid_rebar_config_reports_the_parse_error(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "invalid_rebar.config"),
    Result = lsp_parse:parse_config_file(File, File),
    ?assertMatch(#{parse_result := true, errors_warnings := [_]}, Result),
    #{errors_warnings := [Item]} = Result,
    ?assertEqual(#{type => <<"error">>, file => list_to_binary(File)}, maps:without([info], Item)),
    ?assert(maps:is_key(line, maps:get(info, Item))),
    ?assert(maps:is_key(message, maps:get(info, Item))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wire-level: severity mapping, range, and data shape %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This drives the real lsp_handlers:textDocument_didOpen/2 (exported, no
%% Socket-shaped mock needed beyond a genuine connected gen_tcp pair) so the
%% publishDiagnostics notification it pushes reflects lsp_handlers.erl's
%% actual severity/1 and get_range/1 logic, not a re-implementation of it.
%%
%% Since task 2.1, `data` is the wire (JSON-round-tripped) form of
%% lsp_syntax:correlation_data/1 - before 2.1 this was always `null`.
diagnostics_pin_severity_range_and_data(Config) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, "unused_function.erl"),
    {ok, Text} = file:read_file(File),
    {ServerSocket, ClientSocket} = open_socket_pair(),
    Params = #{textDocument => #{uri => lsp_utils:file_to_file_uri(File), text => Text}},
    lsp_handlers:textDocument_didOpen(ServerSocket, Params),
    Notification = recv_message(ClientSocket, 5000),
    gen_tcp:close(ServerSocket),
    gen_tcp:close(ClientSocket),
    ?assertMatch(#{method := <<"textDocument/publishDiagnostics">>}, Notification),
    #{params := #{diagnostics := [Diagnostic | _]}} = Notification,
    ?assertMatch(
        #{severity := 2, source := <<"erl">>,
          data := #{module := <<"erl_lint">>,
                    messageBody := [<<"unused_function">>, [<<"unused">>, 0]]}},
        Diagnostic
    ),
    %% CHARACTERIZATION: lsp_syntax never provides a line_end/character_end,
    %% so lsp_handlers:get_range/1 always defaults to the same start/end line
    %% and a fixed end column of 255 (0-based; lsp_handlers.erl:490-495,
    %% lsp_utils:client_range/4) - not the real token end.
    ?assertMatch(
        #{range := #{start := #{line := 9, character := _}, 'end' := #{line := 9, character := 255}}},
        Diagnostic
    ).

open_socket_pair() ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(LSock),
    {ok, Client} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, raw}], 2000),
    {ok, Server} = gen_tcp:accept(LSock),
    gen_tcp:close(LSock),
    {Server, Client}.

%% Content-Length framed reader, mirroring gen_lsp_server:handle_tcp_data/3
%% on the client side (same shape as lsp_protocol_SUITE's client helper).
recv_message(Socket, Timeout) ->
    recv_message(Socket, <<>>, undefined, Timeout).

recv_message(_Socket, Buffer, Length, _Timeout) when Length =/= undefined, byte_size(Buffer) >= Length ->
    Body = binary:part(Buffer, 0, Length),
    {ok, Term, _} = vscode_jsone_decode:decode(Body, [{keys, atom}]),
    Term;
recv_message(Socket, Buffer, undefined, Timeout) ->
    case binary:match(Buffer, <<"\r\n\r\n">>) of
        nomatch ->
            {ok, Data} = gen_tcp:recv(Socket, 0, Timeout),
            recv_message(Socket, <<Buffer/binary, Data/binary>>, undefined, Timeout);
        {Pos, Len} ->
            {match, [_, {LStart, LLen}]} = re:run(Buffer, "Content-Length: *([0-9]+)"),
            Length = binary_to_integer(binary:part(Buffer, LStart, LLen)),
            BodyStart = Pos + Len,
            Rest = binary:part(Buffer, BodyStart, byte_size(Buffer) - BodyStart),
            recv_message(Socket, Rest, Length, Timeout)
    end;
recv_message(Socket, Buffer, Length, Timeout) ->
    {ok, Data} = gen_tcp:recv(Socket, 0, Timeout),
    recv_message(Socket, <<Buffer/binary, Data/binary>>, Length, Timeout).
