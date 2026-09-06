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
    bad_record_field_is_reported,
    valid_app_src_parses_cleanly,
    invalid_app_src_reports_the_parse_error,
    valid_rebar_config_parses_cleanly,
    invalid_rebar_config_reports_the_parse_error,
    diagnostics_pin_severity_range_and_null_data
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

bad_record_field_is_reported(Config) ->
    Item = the_one_diagnostic(Config, "bad_record_field.erl"),
    ?assertMatch(#{type := <<"error">>}, Item),
    #{info := Info} = Item,
    ?assertEqual(6, maps:get(line, Info)),
    assert_message_contains(Info, "rec").

%% CHARACTERIZATION: lsp_syntax never sets a `data`/`correlation_data` field
%% on any errors_warnings item - lsp_handlers:send_diagnostics/3 always
%% forwards `null` for it today (lsp_handlers.erl:484). Phase 2 (code actions)
%% is expected to start populating this so a fix can be matched back to its
%% diagnostic without re-analysis; this pins the "nothing there yet" baseline.
the_one_diagnostic(Config, FileName) ->
    AppDir = ?config(data_dir, Config),
    File = filename:join(AppDir, FileName),
    Result = lsp_syntax:validate_parsed_source_file(File),
    ?assertMatch(#{parse_result := true, errors_warnings := [_ | _]}, Result),
    #{errors_warnings := [Item | _]} = Result,
    ?assertEqual(lists:sort([type, file, info]), lists:sort(maps:keys(Item))),
    ?assertEqual(list_to_binary(File), maps:get(file, Item)),
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
diagnostics_pin_severity_range_and_null_data(Config) ->
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
        #{severity := 2, source := <<"erl">>, data := null},
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
