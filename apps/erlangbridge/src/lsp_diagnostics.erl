-module(lsp_diagnostics).
-behavior(gen_server).

%% Coordination point for the two diagnostic paths that must not fight each
%% other: the push path (textDocument/publishDiagnostics) and the LSP 3.17
%% pull path (textDocument/diagnostic, workspace/diagnostic).
%%
%% Deliberately a process of its own rather than state on gen_lsp_server:
%% gen_lsp_server owns the socket and parks in gen_tcp:accept/1, so it cannot
%% also service casts promptly, and tests that exercise diagnostics have no way
%% to stand one up.

%% API
-export([start_link/0]).
-export([schedule_refresh/1, wait_for_change/1, notify_changed/0]).
-export([schedule_configuration_request/2]).
-export([schedule_validation/2, cancel_validation/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lsp_log.hrl").

-define(SERVER, ?MODULE).

%% A burst of pushes is coalesced into a single workspace/diagnostic/refresh.
%% One refresh per push restarts the client's whole workspace pull every time.
-define(REFRESH_DEBOUNCE_MS, 300).

%% A burst of workspace/didChangeConfiguration notifications (VS Code can
%% fire Workspace.onDidChangeConfiguration more than once per logical
%% settings change) is coalesced into a single workspace/configuration
%% round-trip. Without this, each firing independently races its own full
%% project rescan and per-document revalidation - see
%% lsp_handlers:configuration/2's "Why validate_file alone is enough" note.
-define(CONFIGURATION_DEBOUNCE_MS, 300).

%% Keystrokes inside a textDocument/didChange burst are coalesced into a
%% single validation per file. Open documents are reported through the push
%% channel only (see lsp_handlers:workspace_diagnostic_item/2), so a change
%% has to revalidate to keep unsaved edits - a quick fix's WorkspaceEdit
%% among them - from leaving a fixed problem in the Problems list; this is
%% what keeps that off the per-keystroke path.
-define(VALIDATION_DEBOUNCE_MS, 400).

-record(state, {refresh_timer, refresh_socket, waiters = [],
                configuration_timer, configuration_socket, configuration_source,
                validation_timers = #{} :: #{file:filename() => reference()}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Ask for one workspace/diagnostic/refresh, debounced.
schedule_refresh(Socket) ->
    gen_server:cast(?SERVER, {schedule_refresh, Socket}).

%% @doc Block the calling process until diagnostics change anywhere, or Timeout
%% elapses. lsp_handlers:workspace_diagnostic/2 uses this to honour the
%% workspace pull being a long poll - answering an all-unchanged pull at once
%% makes the client re-pull immediately, spinning the server.
wait_for_change(Timeout) ->
    Ref = make_ref(),
    gen_server:cast(?SERVER, {register_waiter, self(), Ref}),
    receive
        {diagnostics_changed, Ref} -> changed
    after Timeout ->
        timeout
    end.

notify_changed() ->
    gen_server:cast(?SERVER, notify_changed).

%% @doc Ask for one workspace/configuration round-trip, debounced. Source
%% identifies the triggering notification (rides into the request id via
%% gen_lsp_server:next_request_id/1, see lsp_handlers:request_configuration/2)
%% - a later call within the debounce window keeps whichever Source arrived
%% last, since that is the one whose round-trip will actually be sent.
schedule_configuration_request(Socket, Source) ->
    gen_server:cast(?SERVER, {schedule_configuration_request, Socket, Source}).

%% @doc Ask for one validation (lint + publishDiagnostics) of File, debounced
%% per file - a later call within the window restarts the wait, so a burst of
%% edits lints once, at the end.
schedule_validation(Socket, File) ->
    gen_server:cast(?SERVER, {schedule_validation, Socket, File}).

%% @doc Drop any validation still pending for File. Called when the document
%% closes: textDocument/didClose clears the file's push diagnostics, and a
%% debounced validation landing just after that would publish them straight
%% back - for a file the workspace pull has meanwhile taken back over, i.e.
%% the duplicate all over again.
cancel_validation(File) ->
    gen_server:cast(?SERVER, {cancel_validation, File}).

init(_Args) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({schedule_refresh, Socket}, #state{refresh_timer = undefined} = State) ->
    Timer = erlang:send_after(?REFRESH_DEBOUNCE_MS, self(), send_refresh),
    {noreply, State#state{refresh_timer = Timer, refresh_socket = Socket}};
handle_cast({schedule_refresh, Socket}, State) ->
    %% A refresh is already pending - coalesce into it.
    {noreply, State#state{refresh_socket = Socket}};

handle_cast({register_waiter, Pid, Ref}, State) ->
    {noreply, State#state{waiters = [{Pid, Ref} | State#state.waiters]}};

handle_cast({schedule_configuration_request, Socket, Source}, #state{configuration_timer = undefined} = State) ->
    Timer = erlang:send_after(?CONFIGURATION_DEBOUNCE_MS, self(), send_configuration_request),
    {noreply, State#state{configuration_timer = Timer, configuration_socket = Socket,
                          configuration_source = Source}};
handle_cast({schedule_configuration_request, Socket, Source}, State) ->
    %% A request is already pending - coalesce into it.
    {noreply, State#state{configuration_socket = Socket, configuration_source = Source}};

handle_cast({schedule_validation, Socket, File}, #state{validation_timers = Timers} = State) ->
    case maps:get(File, Timers, undefined) of
        undefined -> ok;
        Pending -> erlang:cancel_timer(Pending)
    end,
    Timer = erlang:send_after(?VALIDATION_DEBOUNCE_MS, self(), {validate, Socket, File}),
    {noreply, State#state{validation_timers = Timers#{File => Timer}}};

handle_cast({cancel_validation, File}, #state{validation_timers = Timers} = State) ->
    case maps:get(File, Timers, undefined) of
        undefined -> ok;
        Pending -> erlang:cancel_timer(Pending)
    end,
    %% A {validate, ...} message already in this process's own mailbox is
    %% dropped by handle_info's cancelled clause below, which is why the
    %% timers map is the authority on what is still wanted.
    {noreply, State#state{validation_timers = maps:remove(File, Timers)}};

handle_cast(notify_changed, State) ->
    lists:foreach(fun ({Pid, Ref}) ->
        is_process_alive(Pid) andalso (Pid ! {diagnostics_changed, Ref})
    end, State#state.waiters),
    {noreply, State#state{waiters = []}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(send_refresh, #state{refresh_socket = undefined} = State) ->
    {noreply, State#state{refresh_timer = undefined}};
handle_info(send_refresh, #state{refresh_socket = Socket} = State) ->
    ?LOG(<<"diag">>, "request_diagnostic_refresh: asking client to re-pull diagnostics", []),
    gen_lsp_server:send_to_client(Socket, <<"workspace/diagnostic/refresh">>, #{
        id => gen_lsp_server:next_request_id(<<"workspace_diagnostic_refresh">>),
        method => <<"workspace/diagnostic/refresh">>,
        params => null
    }),
    {noreply, State#state{refresh_timer = undefined}};
%% Linting a large file is slow and this process also services the workspace
%% pull's waiters, so the validation itself runs outside it - like
%% gen_lsp_server does for every incoming message.
handle_info({validate, Socket, File}, #state{validation_timers = Timers} = State) ->
    case maps:is_key(File, Timers) of
        false ->
            %% cancel_validation/1 came in after this timer had already fired
            ?LOG(<<"diag">>, "schedule_validation: dropping cancelled validation of ~p", [File]),
            {noreply, State};
        true ->
            ?LOG(<<"diag">>, "schedule_validation: debounce elapsed, validating ~p", [File]),
            spawn(fun () -> lsp_handlers:validate_file(Socket, File) end),
            {noreply, State#state{validation_timers = maps:remove(File, Timers)}}
    end;
handle_info(send_configuration_request, #state{configuration_socket = undefined} = State) ->
    {noreply, State#state{configuration_timer = undefined}};
handle_info(send_configuration_request, #state{configuration_socket = Socket,
                                                configuration_source = Source} = State) ->
    lsp_handlers:request_configuration(Socket, Source),
    {noreply, State#state{configuration_timer = undefined, configuration_socket = undefined,
                          configuration_source = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
