%% Task 6.3: an eunit_listener that streams per-test progress to the LSP
%% client as `erlang/testRunProgress` notifications, and records the final
%% per-test status in an ETS table so the caller (lsp_testing:run_tests/2)
%% can read it back once eunit:test/2 returns.
%%
%% This is a sibling of eunit_jsonreport.erl (same behaviour, same `Data`
%% proplist shape from eunit's event manager - see `source`/`status`/`line`
%% below), not a reuse of it verbatim: eunit_jsonreport only ever writes one
%% final file via a hand-rolled string builder, which doesn't fit streaming
%% per-test results as they happen.
-module(lsp_testing_eunit_report).

-behaviour(eunit_listener).

-export([start/1]).
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

-record(state, {socket, table}).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(Options) ->
    Socket = proplists:get_value(socket, Options),
    Table = proplists:get_value(result_table, Options),
    State = #state{socket = Socket, table = Table},
    receive
        {start, _Reference} -> State
    end.

handle_begin(test, Data, State) ->
    notify(State, Data, <<"running">>, undefined),
    State;
handle_begin(_Kind, _Data, State) ->
    State.

handle_end(test, Data, State) ->
    {StatusBin, Message} = classify(proplists:get_value(status, Data)),
    record(State, Data, StatusBin, Message),
    notify(State, Data, StatusBin, Message),
    State;
handle_end(_Kind, _Data, State) ->
    State.

handle_cancel(test, Data, State) ->
    Message = lsp_utils:to_binary(io_lib:format("~p", [proplists:get_value(reason, Data)])),
    record(State, Data, <<"skipped">>, Message),
    notify(State, Data, <<"skipped">>, Message),
    State;
handle_cancel(_Kind, _Data, State) ->
    State.

terminate(_Result, _State) ->
    ok.

classify(ok) ->
    {<<"passed">>, undefined};
classify({error, Exception}) ->
    {<<"failed">>, lsp_utils:to_binary(io_lib:format("~p", [Exception]))};
classify(_Other) ->
    {<<"failed">>, undefined}.

record(#state{table = Table}, Data, StatusBin, Message) ->
    case proplists:get_value(source, Data) of
        undefined ->
            ok;
        Source ->
            Line = proplists:get_value(line, Data),
            ets:insert(Table, {Source, StatusBin, Message, Line})
    end.

notify(#state{socket = Socket}, Data, StatusBin, Message) ->
    case proplists:get_value(source, Data) of
        undefined ->
            ok;
        {Module, Function, Arity} ->
            Line = proplists:get_value(line, Data),
            gen_lsp_server:send_to_client(Socket, #{
                method => <<"erlang/testRunProgress">>,
                params => #{
                    kind => <<"eunit">>,
                    module => lsp_utils:to_binary(Module),
                    function => lsp_utils:to_binary(Function),
                    arity => Arity,
                    status => StatusBin,
                    message => optional(Message),
                    line => optional(Line)
                }
            })
    end.

optional(undefined) -> null;
optional(Value) -> Value.
