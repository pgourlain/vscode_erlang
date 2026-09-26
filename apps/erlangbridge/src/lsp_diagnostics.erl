-module(lsp_diagnostics).
-behavior(gen_server).

%% Schedules every lint whose result is pushed to the client
%% (textDocument/publishDiagnostics - the only diagnostics channel, the server
%% advertises no pull support):
%%
%% - open documents: one debounced validation per file (schedule_validation/2),
%%   run as soon as the debounce elapses;
%% - closed project files: a background queue linted one file at a time
%%   (schedule_background_validation/2, schedule_disk_validation/2), so a
%%   project scan never floods the VM with hundreds of concurrent lints.
%%
%% It also remembers which files currently show diagnostics, so that files
%% leaving the project, or everything when erlang.linting is turned off, can
%% be cleared: with push only, the client keeps whatever it was last sent.
%%
%% Deliberately a process of its own rather than state on gen_lsp_server:
%% gen_lsp_server owns the socket and parks in gen_tcp:accept/1, so it cannot
%% also service casts promptly, and tests that exercise diagnostics have no way
%% to stand one up. Lints themselves never run in here, nor in
%% gen_lsp_doc_server or gen_lsp_server.

%% API
-export([start_link/0]).
-export([schedule_configuration_request/2]).
-export([schedule_validation/2, cancel_validation/1]).
-export([schedule_background_validation/2, schedule_disk_validation/2]).
-export([project_scanned/2, clear_all/1, published/2]).
%% For lsp_diagnostic_SUITE: nothing pending, nothing running.
-export([idle/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("lsp_log.hrl").

-define(SERVER, ?MODULE).

%% A burst of workspace/didChangeConfiguration notifications (VS Code can
%% fire Workspace.onDidChangeConfiguration more than once per logical
%% settings change) is coalesced into a single workspace/configuration
%% round-trip. Without this, each firing independently races its own full
%% project rescan and per-document revalidation - see
%% lsp_handlers:configuration/2's "Why validate_file alone is enough" note.
-define(CONFIGURATION_DEBOUNCE_MS, 300).

%% Keystrokes inside a textDocument/didChange burst are coalesced into a
%% single validation per file, so that an unsaved edit - a quick fix's
%% WorkspaceEdit among them - republishes the file's diagnostics without
%% linting on every keystroke.
-define(VALIDATION_DEBOUNCE_MS, 400).

%% `cached` lints the syntax tree gen_lsp_doc_server already holds (parsed on
%% demand if missing); `disk` reparses the file from disk first, for when it is
%% known to have changed under a tree still stamped as current.
-type mode() :: cached | disk.

-record(state, {configuration_timer, configuration_socket, configuration_source,
                validation_timers = #{} :: #{file:filename() => reference()},
                background = [] :: [{file:filename(), mode()}],
                background_socket,
                running :: undefined | file:filename(),
                published = #{} :: #{file:filename() => true}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Ask for one workspace/configuration round-trip, debounced. Source
%% identifies the triggering notification (rides into the request id via
%% gen_lsp_server:next_request_id/1, see lsp_handlers:request_configuration/2)
%% - a later call within the debounce window keeps whichever Source arrived
%% last, since that is the one whose round-trip will actually be sent.
schedule_configuration_request(Socket, Source) ->
    gen_server:cast(?SERVER, {schedule_configuration_request, Socket, Source}).

%% @doc Ask for one validation (lint + publishDiagnostics) of the open
%% document File, debounced per file - a later call within the window
%% restarts the wait, so a burst of edits lints once, at the end.
schedule_validation(Socket, File) ->
    gen_server:cast(?SERVER, {schedule_validation, Socket, File}).

%% @doc Drop any debounced validation still pending for File. Called when the
%% document closes: from then on the file is linted from disk, through the
%% background queue.
cancel_validation(File) ->
    gen_server:cast(?SERVER, {cancel_validation, File}).

%% @doc Queue closed project files for linting, one at a time, behind whatever
%% is already queued. A file already queued keeps its place; a file open by
%% the time its turn comes is skipped, its own push path owns it.
schedule_background_validation(Socket, Files) ->
    gen_server:cast(?SERVER, {schedule_background_validation, Socket, Files, cached, back}).

%% @doc Lint File from disk ahead of the rest of the queue: a document just
%% closed, or a closed file created, changed or deleted on disk. A file that
%% no longer exists has its diagnostics cleared.
schedule_disk_validation(Socket, File) ->
    gen_server:cast(?SERVER, {schedule_background_validation, Socket, [File], disk, front}).

%% @doc The project scan has finished and found Files: lint each of them in
%% the background, and clear any file still showing diagnostics that is no
%% longer part of the project (e.g. newly excluded by search.exclude) - unless
%% it is open, in which case its buffer still owns what it shows.
project_scanned(Socket, Files) ->
    gen_server:cast(?SERVER, {project_scanned, Socket, Files}).

%% @doc erlang.linting was turned off: drop every pending lint and clear every
%% file still showing diagnostics.
clear_all(Socket) ->
    gen_server:cast(?SERVER, {clear_all, Socket}).

%% @doc Bookkeeping for lsp_handlers:send_diagnostics/3 - which files are
%% currently showing something, so they can be cleared later.
published(File, Diagnostics) ->
    gen_server:cast(?SERVER, {published, File, Diagnostics =/= []}).

idle() ->
    gen_server:call(?SERVER, idle).

init(_Args) ->
    {ok, #state{}}.

handle_call(idle, _From, #state{validation_timers = Timers, background = Queue, running = Running} = State) ->
    {reply, map_size(Timers) =:= 0 andalso Queue =:= [] andalso Running =:= undefined, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({schedule_configuration_request, Socket, Source}, #state{configuration_timer = undefined} = State) ->
    Timer = erlang:send_after(?CONFIGURATION_DEBOUNCE_MS, self(), send_configuration_request),
    {noreply, State#state{configuration_timer = Timer, configuration_socket = Socket,
                          configuration_source = Source}};
handle_cast({schedule_configuration_request, Socket, Source}, State) ->
    %% A request is already pending - coalesce into it.
    {noreply, State#state{configuration_socket = Socket, configuration_source = Source}};

handle_cast({schedule_validation, Socket, File}, #state{validation_timers = Timers} = State) ->
    cancel_timer(maps:get(File, Timers, undefined)),
    Timer = erlang:send_after(?VALIDATION_DEBOUNCE_MS, self(), {validate, Socket, File}),
    {noreply, State#state{validation_timers = Timers#{File => Timer}}};

handle_cast({cancel_validation, File}, #state{validation_timers = Timers} = State) ->
    cancel_timer(maps:get(File, Timers, undefined)),
    %% A {validate, ...} message already in this process's own mailbox is
    %% dropped by handle_info's cancelled clause below, which is why the
    %% timers map is the authority on what is still wanted.
    {noreply, State#state{validation_timers = maps:remove(File, Timers)}};

handle_cast({schedule_background_validation, Socket, Files, Mode, Where}, State) ->
    Queue = enqueue(Files, Mode, Where, State#state.background),
    {noreply, run_next(State#state{background = Queue, background_socket = Socket})};

handle_cast({project_scanned, Socket, Files}, #state{published = Published} = State) ->
    Opened = gen_lsp_doc_server:opened_documents(),
    Gone = [File || File <- maps:keys(Published),
                    not lists:member(File, Files), not lists:member(File, Opened)],
    ?LOG(<<"diag">>, "project_scanned: ~p files to lint, clearing ~p", [length(Files), Gone]),
    lists:foreach(fun (File) -> lsp_handlers:send_diagnostics(Socket, File, []) end, Gone),
    Queue = enqueue(Files, cached, back, [Item || {File, _} = Item <- State#state.background,
                                                  not lists:member(File, Gone)]),
    {noreply, run_next(State#state{background = Queue, background_socket = Socket,
                                   published = maps:without(Gone, Published)})};

handle_cast({clear_all, Socket}, #state{published = Published, validation_timers = Timers} = State) ->
    ?LOG(<<"diag">>, "clear_all: linting disabled, clearing ~p", [maps:keys(Published)]),
    lists:foreach(fun cancel_timer/1, maps:values(Timers)),
    lists:foreach(fun (File) -> lsp_handlers:send_diagnostics(Socket, File, []) end, maps:keys(Published)),
    %% A lint already running is not stopped: validate_file/2 checks
    %% erlang.linting before it publishes anything.
    {noreply, State#state{background = [], validation_timers = #{}, published = #{}}};

handle_cast({published, File, true}, #state{published = Published} = State) ->
    {noreply, State#state{published = Published#{File => true}}};
handle_cast({published, File, false}, #state{published = Published} = State) ->
    {noreply, State#state{published = maps:remove(File, Published)}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% Linting a large file is slow and this process has to stay responsive, so
%% the validation itself runs outside it - like gen_lsp_server does for every
%% incoming request.
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
handle_info({worker_result, {background, File}, _Result}, #state{running = File} = State) ->
    {noreply, run_next(State#state{running = undefined})};
handle_info({worker_error, {background, File}, Error}, #state{running = File} = State) ->
    lsp_log:error(<<"diag">>, "background validation of ~p failed: ~p", [File, Error]),
    {noreply, run_next(State#state{running = undefined})};
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

cancel_timer(undefined) -> ok;
cancel_timer(Timer) -> erlang:cancel_timer(Timer).

%% Deduplicated: a file already queued stays queued once, `disk` winning over
%% `cached` (a reparse covers a plain lint, not the other way around), and
%% moved to the front when asked for there.
enqueue(Files, Mode, Where, Queue) ->
    lists:foldl(fun (File, Acc) ->
        {Mode2, Rest} = case lists:keytake(File, 1, Acc) of
            {value, {File, disk}, Others} -> {disk, Others};
            {value, {File, cached}, Others} -> {Mode, Others};
            false -> {Mode, Acc}
        end,
        case {Where, Rest =:= Acc} of
            {front, _} -> [{File, Mode2} | Rest];
            %% already queued: keep its place, only the mode may change
            {back, false} -> lists:keystore(File, 1, Acc, {File, Mode2});
            {back, true} -> Acc ++ [{File, Mode2}]
        end
    end, Queue, case Where of front -> lists:reverse(Files); back -> Files end).

run_next(#state{running = undefined, background = [{File, Mode} | Rest],
                background_socket = Socket} = State) ->
    case gen_lsp_doc_server:get_document_contents(File) of
        undefined ->
            worker:start(fun () -> lsp_handlers:validate_closed_file(Socket, File, Mode) end,
                         {background, File}),
            State#state{running = File, background = Rest};
        _Open ->
            %% Opened since it was queued - its own push path lints the buffer.
            run_next(State#state{background = Rest})
    end;
run_next(State) ->
    State.
