-module(lsp_protocol_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-include("./testlog.hrl").

%% Protocol-level characterization net: talks to a real gen_lsp_server over
%% TCP (Content-Length framing + vscode_jsone), the same way vscode-languageclient
%% does. Module-level suites (lsp_navigation_SUITE and friends) call analysis
%% modules directly and can never reach lsp_handlers, whose functions take a
%% Socket - this suite is what makes edits to lsp_handlers.erl / gen_lsp_server.erl
%% safe.
%%
%% Each test case runs the whole vscode_lsp application on its own `peer` node
%% (OTP >= 25) instead of in the common_test node itself. That is required,
%% not just tidy isolation: shutdown/2 and exit/2 both call init:stop()
%% unconditionally (see lsp_handlers.erl), which would otherwise kill the
%% common_test run itself.

all() -> [
    initialize_returns_golden_capabilities,
    unknown_method_returns_handler_error,
    cancel_request_is_a_silent_notification,
    set_trace_is_a_silent_notification,
    shutdown_closes_the_connection,
    exit_closes_the_connection,
    initialized_requests_configuration_with_a_tagged_unique_id,
    did_change_configuration_burst_collapses_to_one_configuration_call
].

init_per_testcase(_TestCase, Config) ->
    {ok, Peer, Node} = start_lsp_peer(),
    {Addr, Port} = wait_for_lsp_listener(Peer, 50),
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}, {packet, raw}], 2000),
    [{peer, Peer}, {node, Node}, {socket, Socket} | Config].

end_per_testcase(_TestCase, Config) ->
    catch gen_tcp:close(?config(socket, Config)),
    catch peer:stop(?config(peer, Config)),
    ok.

%%%%%%%%%%%%%%%%
%% test cases %%
%%%%%%%%%%%%%%%%

%% Pins the full capability map verbatim (lsp_handlers.erl:14-53; updated
%% for task 1.3's workspace.workspaceFolders and task 2.1's codeAction/
%% executeCommand infrastructure) so any
%% later capability flip is a visible, deliberate diff to this golden term -
%% not an accidental side effect of an unrelated change.
initialize_returns_golden_capabilities(Config) ->
    Socket = ?config(socket, Config),
    ok = send_request(Socket, <<"initialize">>, 1, #{rootPath => null}),
    ?assertEqual(#{id => 1, result => golden_initialize_result()}, recv_message(Socket, 5000)).

%% A method with no exported lsp_handlers function must fail with -32001,
%% not crash the connection or go unanswered.
unknown_method_returns_handler_error(Config) ->
    Socket = ?config(socket, Config),
    ok = send_request(Socket, <<"totally/unknownMethod">>, 42, #{}),
    ?assertEqual(
        #{id => 42, error => #{code => -32001, message => <<"Method not handled">>}},
        recv_message(Socket, 5000)
    ).

%% $/cancelRequest is a notification (no id): the server must not reply to
%% it, and must stay responsive to further requests on the same connection.
cancel_request_is_a_silent_notification(Config) ->
    Socket = ?config(socket, Config),
    ok = send_notification(Socket, <<"$/cancelRequest">>, #{id => 1}),
    assert_connection_still_responsive(Socket).

%% Same characterization for $/setTrace: accepted silently, connection stays up.
set_trace_is_a_silent_notification(Config) ->
    Socket = ?config(socket, Config),
    ok = send_notification(Socket, <<"$/setTrace">>, #{value => <<"off">>}),
    assert_connection_still_responsive(Socket).

%% CHARACTERIZATION: shutdown/2 calls init:stop() directly instead of just
%% flagging the server as shutting down and waiting for `exit` (the LSP spec
%% shape). Observable effect today: the TCP connection is torn down almost
%% immediately, because init:stop() takes the whole node down.
shutdown_closes_the_connection(Config) ->
    Socket = ?config(socket, Config),
    ok = send_request(Socket, <<"shutdown">>, 7, #{}),
    ?assertEqual({error, closed}, wait_for_socket_close(Socket, 5000)).

%% CHARACTERIZATION: exit/2 also calls init:stop() unconditionally, even
%% without a prior shutdown request.
exit_closes_the_connection(Config) ->
    Socket = ?config(socket, Config),
    ok = send_request(Socket, <<"exit">>, 8, #{}),
    ?assertEqual({error, closed}, wait_for_socket_close(Socket, 5000)).

%% Server-initiated requests used to share a constant JSON-RPC id, which is a
%% spec violation the moment two are in flight (three refresh requests went out
%% under one id, one response came back) and left no way to tell which call site
%% asked. Ids now carry provenance plus a sequence, and the response is routed
%% back on the base id alone.
initialized_requests_configuration_with_a_tagged_unique_id(Config) ->
    Socket = ?config(socket, Config),
    ok = send_notification(Socket, <<"initialized">>, #{}),
    Request = recv_message(Socket, 5000),
    ?assertMatch(#{method := <<"workspace/configuration">>}, Request),
    Id = maps:get(id, Request),
    ?assertMatch({match, _}, re:run(Id, "^configuration#initialized#[0-9]+$")),

    %% Answer as the client would - the tagged id must still reach
    %% lsp_handlers:configuration/2 rather than falling through as unhandled.
    ok = send_message(Socket, #{jsonrpc => <<"2.0">>, id => Id,
                                result => [#{verbose => false}, #{}, #{}, #{}, #{}]}),
    assert_connection_still_responsive(Socket).

%% VS Code's Workspace.onDidChangeConfiguration can fire more than once for a
%% single logical settings change (once per settings scope that resolves) -
%% each firing independently sending this notification used to launch its own
%% full workspace/configuration round-trip, racing full project rescans and
%% per-document revalidations against each other (see
%% lsp_handlers:workspace_didChangeConfiguration/2's doc comment, and the
%% "diagnostic desynchronised after a manual edit" bug that race caused). A
%% burst must collapse into a single request.
did_change_configuration_burst_collapses_to_one_configuration_call(Config) ->
    Socket = ?config(socket, Config),
    lists:foreach(fun (_) ->
        ok = send_notification(Socket, <<"workspace/didChangeConfiguration">>, #{settings => null})
    end, lists:seq(1, 5)),

    Request = recv_message(Socket, 5000),
    ?assertMatch(#{method := <<"workspace/configuration">>}, Request),
    Id = maps:get(id, Request),
    ?assertMatch({match, _}, re:run(Id, "^configuration#didChangeConfiguration#[0-9]+$")),

    %% Well past the debounce window: nothing else was sent - five
    %% notifications collapsed into this one request.
    ?assertEqual({error, timeout}, gen_tcp:recv(Socket, 0, 1000)),

    ok = send_message(Socket, #{jsonrpc => <<"2.0">>, id => Id,
                                result => [#{verbose => false}, #{}, #{}, #{}, #{}]}),
    assert_connection_still_responsive(Socket).

%%%%%%%%%%%%%%%%%%%%%%
%% golden reference %%
%%%%%%%%%%%%%%%%%%%%%%

golden_initialize_result() ->
    #{capabilities => #{
        textDocumentSync => 2, %% Incremental, task 1.4
        completionProvider => #{triggerCharacters => <<":#.?-">>, resolveProvider => true}, %% task 5.6
        hoverProvider => true,
        signatureHelpProvider => #{triggerCharacters => <<"(,">>, retriggerCharacters => <<",">>},
        declarationProvider => true, %% task 4.2
        definitionProvider => true,
        typeDefinitionProvider => true, %% task 4.3
        implementationProvider => true, %% task 4.4
        referencesProvider => true,
        documentHighlightProvider => true, %% task 4.7
        documentSymbolProvider => true,
        codeActionProvider => #{ %% task 2.1
            codeActionKinds => [<<"quickfix">>, <<"source">>, <<"refactor">>],
            resolveProvider => true
        },
        codeLensProvider => #{resolveProvider => true}, %% task 5.10
        documentLinkProvider => #{resolveProvider => false}, %% task 5.7
        colorProvider => false,
        documentFormattingProvider => true,
        documentRangeFormattingProvider => true, %% task 5.3
        documentOnTypeFormattingProvider => #{ %% task 5.4
            firstTriggerCharacter => <<".">>,
            moreTriggerCharacter => [<<";">>, <<",">>, <<"\n">>]
        },
        renameProvider => #{prepareProvider => true},
        foldingRangeProvider => true, %% task 5.1
        executeCommandProvider => #{commands => []}, %% task 2.1, no commands registered yet
        selectionRangeProvider => true, %% task 5.2
        linkedEditingRangeProvider => false,
        callHierarchyProvider => true, %% task 4.5
        semanticTokensProvider => #{ %% task 3.1, range/delta task 3.2
            legend => #{
                tokenTypes => [<<"namespace">>, <<"function">>, <<"macro">>, <<"variable">>,
                               <<"parameter">>, <<"type">>, <<"struct">>, <<"property">>,
                               <<"string">>, <<"number">>, <<"comment">>, <<"keyword">>, <<"operator">>],
                tokenModifiers => [<<"definition">>, <<"declaration">>, <<"readonly">>,
                                   <<"deprecated">>, <<"defaultLibrary">>]
            },
            full => #{delta => true},
            range => true
        },
        monikerProvider => false,
        typeHierarchyProvider => true, %% task 4.6
        inlineValueProvider => true,
        inlayHintProvider => #{resolveProvider => true}, %% task 5.8
        diagnosticProvider => #{interFileDependencies => true, workspaceDiagnostics => true}, %% task 5.9
        workspaceSymbolProvider => #{resolveProvider => true}, %% task 4.1
        workspace => #{
            workspaceFolders => #{supported => true, changeNotifications => true}
        }
    },
    serverInfo => #{name => <<"vscode_erlang">>, version => expected_otp_version()}}. %% task 7.3

expected_otp_version() ->
    Release = erlang:system_info(otp_release),
    case file:read_file(filename:join([code:root_dir(), "releases", Release, "OTP_VERSION"])) of
        {ok, Bin} -> string:trim(Bin);
        {error, _} -> list_to_binary(Release)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% peer node / TCP helpers %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_lsp_peer() ->
    {ok, Peer, Node} = peer:start_link(#{
        name => peer:random_name(?MODULE),
        args => ["-pa" | code:get_path()],
        connection => standard_io
    }),
    ok = peer:call(Peer, application, start, [vscode_lsp, permanent]),
    _ = peer:call(Peer, gen_lsp_config_server, update_config, [erlang, #{verbose => false}]),
    {ok, Peer, Node}.

%% gen_lsp_sup listens on port 0 (OS-assigned) unless started with a
%% -vscode_port argument. Discover the assigned port the same way
%% gen_connection_SUITE does for the sibling debugger command server: find
%% the loopback TCP listener among erlang:ports/0. The peer node is freshly
%% started for this test case alone, so no before/after diff is needed -
%% any bound, peerless tcp_inet port on it is the LSP listener.
wait_for_lsp_listener(_Peer, 0) ->
    ct:fail(lsp_listener_not_found);
wait_for_lsp_listener(Peer, N) ->
    %% gen_lsp_sup binds specifically to 127.0.0.1 (?TCP_OPTIONS); other
    %% tcp_inet listeners the peer node may hold open (e.g. distribution-
    %% related, bound to 0.0.0.0) are not the LSP socket and must be excluded.
    Listeners = [{Addr, Port} ||
                    P <- peer:call(Peer, erlang, ports, []),
                    peer:call(Peer, erlang, port_info, [P, name]) =:= {name, "tcp_inet"},
                    peer:call(Peer, inet, peername, [P]) =:= {error, enotconn},
                    {ok, {Addr, Port}} <- [peer:call(Peer, inet, sockname, [P])],
                    Addr =:= {127, 0, 0, 1}],
    case Listeners of
        [One] -> One;
        [] ->
            timer:sleep(100),
            wait_for_lsp_listener(Peer, N - 1)
    end.

assert_connection_still_responsive(Socket) ->
    ok = send_request(Socket, <<"probe/stillAlive">>, 99, #{}),
    ?assertEqual(
        #{id => 99, error => #{code => -32001, message => <<"Method not handled">>}},
        recv_message(Socket, 5000)
    ).

send_request(Socket, Method, Id, Params) ->
    send_message(Socket, #{jsonrpc => <<"2.0">>, id => Id, method => Method, params => Params}).

send_notification(Socket, Method, Params) ->
    send_message(Socket, #{jsonrpc => <<"2.0">>, method => Method, params => Params}).

send_message(Socket, Msg) ->
    {ok, Json} = vscode_jsone:encode(Msg),
    Header = iolist_to_binary(io_lib:fwrite("Content-Length: ~p\r\n\r\n", [byte_size(Json)])),
    ok = gen_tcp:send(Socket, [Header, Json]).

%% Content-Length framed reader, mirroring gen_lsp_server:handle_tcp_data/3
%% on the client side. Reads exactly one message.
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

wait_for_socket_close(Socket, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
        {error, closed} -> {error, closed};
        {ok, _Data} -> wait_for_socket_close(Socket, Timeout);
        Other -> Other
    end.
